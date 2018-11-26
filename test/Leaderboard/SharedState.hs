{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Leaderboard.SharedState where

import           Control.Lens           (Getter, Lens', at, lens, makeLenses,
                                         makeWrapped, to, (%~), (&), (?~), (^.),
                                         _Wrapped)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bool              (bool)
import           Data.Functor.Classes   (Ord1)
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import           Data.Time              (LocalTime)
import           Servant.Auth.Client    (Token)
import           Servant.Client         (ClientEnv, ClientM, ServantError (..),
                                         runClientM)

import           Hedgehog               (Command, Concrete (Concrete), Eq1, Gen,
                                         HTraversable (htraverse), PropertyT,
                                         Show1, TestT, Var (Var), concrete,
                                         evalIO, executeParallel,
                                         executeSequential, forAll, property,
                                         test, withRetries)
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty             (TestTree)
import           Test.Tasty.Hedgehog    (testProperty)

import           Leaderboard.TestClient (fromLbToken)
import           Leaderboard.Types      (HasResponsePlayer (..),
                                         RegisterPlayer (..),
                                         ResponsePlayer (..), RqMatch (..))

-- | Whether or not we're running sequential or parallel tests. It is important in
-- parallel tests that we always register the first user in the sequential prefix.
-- This is because
data SeqOrPara =
    Sequential
  | Parallel
  deriving (Eq, Show)

newtype RegFirstState (v :: * -> *) =
  RegFirstState Integer
  deriving (Eq, Show)

data RegisterState (v :: * -> *) =
  RegisterState
  { rsPlayerCount :: Integer
  , rsAdmins :: S.Set (Var TestRsp v)
  } deriving (Eq, Show)

class CanRegisterPlayers (s :: (* -> *) -> *) where
  registerPlayer ::
    Ord1 v
    => s v
    -> RegisterPlayer
    -> Var TestRsp v
    -> s v

instance CanRegisterPlayers RegFirstState where
  registerPlayer (RegFirstState n) _ _ =
    RegFirstState (succ n)

class HasPlayerCount s where
  playerCount :: forall (v :: * -> *). Getter (s v) Integer

instance HasPlayerCount RegFirstState where
  playerCount =
    to (\(RegFirstState n) -> n)

-- | Map emails to players and keep a set of admin emails
data LeaderboardState (v :: * -> *) =
  LeaderboardState
  { _players :: PlayerMap v
  , _admins  :: S.Set (Var TestRsp v)
  , _matches :: MatchMap v
  }
deriving instance Show1 v => Show (LeaderboardState v)
deriving instance Eq1 v => Eq (LeaderboardState v)

-- If we used lens's template haskell to define these classes, they would be polymorphic on both the
-- @s@ and @a@ type parameters. This would result in an ambiguous type for @v@.
class HasPlayers s where
  players :: Lens' (s v) (PlayerMap v)

instance HasPlayers LeaderboardState where
  players = lens _players (\s ps -> s {_players = ps})

instance HasPlayerCount LeaderboardState where
  playerCount =
    players . to length . to fromIntegral

instance CanRegisterPlayers LeaderboardState where
  registerPlayer s lbr@LeaderboardRegistration{..} rsp  =
    s & players . at _lbrEmail ?~ mkPlayerWithRsp lbr rsp
      & bool id (admins %~ S.insert rsp) (_lbrIsAdmin == Just True)

class HasAdmins (s :: (* -> *) -> *) where
  admins :: Lens' (s v) (S.Set (Var TestRsp v))

instance HasAdmins LeaderboardState where
  admins = lens _admins (\s ps -> s {_admins = ps})

emptyState
  :: LeaderboardState (v :: * -> *)
emptyState =
  LeaderboardState M.empty S.empty M.empty

type PlayerMap v = M.Map Text (PlayerWithRsp v)
type MatchMap v =  M.Map (Var Int v) (TestMatch v)

data PlayerWithRsp (v :: * -> *) =
  PlayerWithRsp
  { _pwrRsp      :: Var TestRsp v
  , _pwrEmail    :: Text
  , _pwrUsername :: Text
  , _pwrPassword :: Text
  , _pwrIsAdmin  :: Maybe Bool
  }
  deriving (Eq, Show)

instance HTraversable PlayerWithRsp where
  htraverse f PlayerWithRsp{..} =
    let mkP _pwrRsp = PlayerWithRsp{..}
    in case _pwrRsp of
        (Var vr) -> mkP <$> (Var <$> f vr)

mkPlayerWithRsp
  :: RegisterPlayer
  -> Var TestRsp v
  -> PlayerWithRsp v
mkPlayerWithRsp LeaderboardRegistration{..} rsp =
  let
    _pwrRsp = rsp
    _pwrEmail = _lbrEmail
    _pwrUsername = _lbrUsername
    _pwrPassword = _lbrPassword
    _pwrIsAdmin = _lbrIsAdmin
  in
    PlayerWithRsp{..}

data TestMatch v =
  TestMatch
  { _tmPlayer1      :: Var TestRsp v
  , _tmPlayer2      :: Var TestRsp v
  , _tmPlayer1Score :: Int
  , _tmPlayer2Score :: Int
  , _tmTime         :: LocalTime
  }
  deriving (Eq, Show)

instance HTraversable TestMatch where
  htraverse f TestMatch{..} =
    case (_tmPlayer1, _tmPlayer2) of
      (Var rp1, Var rp2) ->
        let fps = (,) <$> (Var <$> f rp1) <*> (Var <$> f rp2)
         in (\(_tmPlayer1, _tmPlayer2) -> TestMatch{..}) <$> fps

testToRq
  :: TestMatch Concrete
  -> RqMatch
testToRq TestMatch{..} =
  let
    _matchPlayer1 = _tmPlayer1 ^. concreteLens . rspId
    _matchPlayer2 = _tmPlayer2 ^. concreteLens . rspId
    _matchPlayer1Score  = _tmPlayer1Score
    _matchPlayer2Score = _tmPlayer2Score
    _matchTime = _tmTime
  in
    RqMatch{..}

clientToken
  :: HasResponsePlayer s
  => s
  -> Token
clientToken s  =
  s ^. rspToken . to fromLbToken

-- | Swap the left and rights in the `Either` returned by `runClientM` because
--   we're expecting failure.
failureClient
  :: MonadIO m
  => ClientEnv
  -> ClientM a
  -> m (Either a ServantError)
failureClient ce ma =
  either Right Left <$> liftIO (runClientM ma ce)

successClient
  :: MonadIO m
  => ClientEnv
  -> ClientM a
  -> m (Either ServantError a)
successClient ce ma =
  liftIO $ runClientM ma ce

checkCommands
  :: forall state.
     String
  -> IO ()
  -> (forall v. state v)
  -> [Command Gen (PropertyT IO) state]
  -> TestTree
checkCommands name reset initialState commands  =
  testProperty name . property $ do
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands
  evalIO reset
  executeSequential initialState actions

-- | Produces a 'TestTree' for checking 'Command's in parallel.
--
-- 'Gen' can't be any instance of 'MonadGen' here because 'forAll' takes a @Gen a@.
checkCommandsParallel
  :: forall state.
     String
  -> IO ()
  -> (forall v. state v)
  -> [Command Gen (TestT IO) state]
  -> TestTree
checkCommandsParallel name reset initialState commands  =
  testProperty name . withRetries 10 . property $ do
  actions <- forAll $
    Gen.parallel (Range.linear 10 100) (Range.linear 1 10) initialState commands
  test $ do
    evalIO reset
    executeParallel initialState actions

concreteLens ::
  Lens' (Var a Concrete) a
concreteLens =
  lens concrete (const $ Var . Concrete)

newtype TestRsp =
  TestRsp ResponsePlayer
  deriving (Eq, Show, Ord)

instance HasResponsePlayer TestRsp where
  responsePlayer = _Wrapped . responsePlayer
  -- rspId = responsePlayer . rspId
  -- rspToken = responsePlayer . rspToken

instance HasResponsePlayer (Var TestRsp Concrete) where
  responsePlayer = concreteLens . _Wrapped

makeLenses ''PlayerWithRsp

instance HasResponsePlayer (PlayerWithRsp Concrete) where
  responsePlayer = pwrRsp . concreteLens . _Wrapped
  -- rspId = responsePlayer . rspId
  -- rspToken = responsePlayer . rspToken

makeWrapped ''TestRsp
