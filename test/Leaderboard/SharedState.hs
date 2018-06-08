{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}

module Leaderboard.SharedState where

import           Control.Lens           (Lens', abbreviatedFields, lens,
                                         makeLensesWith)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import           Data.Time              (LocalTime)
import           Servant.Auth.Client    (Token)
import           Servant.Client         (ClientEnv, ClientM, ServantError (..),
                                         runClientM)

import           Hedgehog               (Command, Concrete, Eq1, Gen, TestT,
                                         HTraversable (htraverse), MonadGen,
                                         PropertyT, Show1, Var (Var), concrete,
                                         executeParallel, executeSequential,
                                         forAll, property, test, withRetries, evalIO)
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty             (TestTree)
import           Test.Tasty.Hedgehog    (testProperty)

import           Leaderboard.TestClient (fromLbToken)
import           Leaderboard.Types      (ResponsePlayer (..), RqMatch (..))

-- | Whether or not we're running sequential or parallel tests. It is important in
-- parallel tests that we always register the first user in the sequential prefix.
-- This is because
data SeqOrPara =
    Sequential
  | Parallel
  deriving (Eq, Show)

-- | Map emails to players and keep a set of admin emails
data LeaderboardState (v :: * -> *) =
  LeaderboardState
  { _players :: PlayerMap v
  , _admins  :: S.Set Text
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

class HasAdmins (s :: (* -> *) -> *) where
  admins :: Lens' (s v) (S.Set Text)
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
  { _pwrRsp      :: Var ResponsePlayer v
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

data TestMatch v =
  TestMatch
  { _tmPlayer1      :: Var ResponsePlayer v
  , _tmPlayer2      :: Var ResponsePlayer v
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
    _matchPlayer1 = _rspId . concrete $ _tmPlayer1
    _matchPlayer2  = _rspId . concrete $ _tmPlayer2
    _matchPlayer1Score  = _tmPlayer1Score
    _matchPlayer2Score = _tmPlayer2Score
    _matchTime = _tmTime
  in
    RqMatch{..}

clientToken
  :: PlayerWithRsp Concrete
  -> Token
clientToken PlayerWithRsp{..} =
  fromLbToken . _rspToken . concrete $ _pwrRsp

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

genAdminWithRsp
  :: MonadGen n
  => LeaderboardState v
  -> Maybe (n (PlayerWithRsp v))
genAdminWithRsp (LeaderboardState ps as _ms) =
  -- TODO ajmccluskey: be better
  -- Emails in admin _must_ be a subset of those in players. Without a Traversable
  -- instance for Gen I couldn't make this be not partial.
  if null as
  then Nothing
  else pure $ (M.!) ps <$> (Gen.element . S.toList $ as)

genPlayerWithRsp
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n (PlayerWithRsp v))
genPlayerWithRsp ps =
  if null ps
  then Nothing
  else pure . fmap snd . Gen.element . M.toList $ ps

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

makeLensesWith abbreviatedFields ''PlayerWithRsp
