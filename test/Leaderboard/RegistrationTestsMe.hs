-- This is a simplified version of what `RegistrationTests` does.
-- Used for examples in a talk.

{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Leaderboard.RegistrationTestsMe
  ( registrationTestsMe
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bool                 (bool)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Data.Semigroup            ((<>))
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            Concrete, Eq1, GenT,
                                            HTraversable (htraverse), MonadGen,
                                            MonadTest, PropertyT, Show1,
                                            Symbolic, Var (Var), annotateShow,
                                            assert, concrete, eval, evalEither,
                                            evalM, executeSequential, failure,
                                            forAll, property, success, test,
                                            (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import           Leaderboard.Schema        (Player, PlayerT (..))
import qualified Leaderboard.Schema        as LS
import           Leaderboard.SharedState   (PlayerMap, PlayerWithRsp (..),
                                            checkCommands, clientToken,
                                            emptyState, failureClient,
                                            genPlayerWithRsp, successClient)
import           Leaderboard.TestClient    (getPlayerCount, me, register,
                                            registerFirst)
import           Leaderboard.Types         (PlayerCount (..),
                                            RegisterPlayer (..),
                                            ResponsePlayer (..))

data LeaderboardState (v :: * -> *) =
  LeaderboardState
  { _players :: M.Map Text (PlayerWithRsp v)
  , _admins  :: S.Set Text
  }
deriving instance Show1 v => Show (LeaderboardState v)
deriving instance Eq1 v => Eq (LeaderboardState v)

registrationTestsMe
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTestsMe truncateTables env =
    testGroup "registration-count" [
      propRegisterMe env truncateTables
    ]

genRegPlayerRandomAdmin
  :: MonadGen n
  => PlayerMap v
  -> n RegisterPlayer
genRegPlayerRandomAdmin ps =
  let
    genNonEmptyUnicode = Gen.text (Range.linear 1 20) Gen.alphaNum
  in
    LeaderboardRegistration
      <$> Gen.filter (`S.notMember` M.keysSet ps) genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> Gen.maybe Gen.bool

genAdminWithRsp
  :: MonadGen n
  => LeaderboardState v
  -> Maybe (n (PlayerWithRsp v))
genAdminWithRsp (LeaderboardState ps as) =
  if null as
  then Nothing
  else pure $ (M.!) ps <$> (Gen.element . S.toList $ as)

mkPlayerWithRsp
  :: RegisterPlayer
  -> Var ResponsePlayer v
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

--------------------------------------------------------------------------------
-- PLAYER COUNT
--------------------------------------------------------------------------------

data GetPlayerCount (v :: * -> *) =
    GetPlayerCount
  deriving (Eq, Show)
instance HTraversable GetPlayerCount where
  htraverse _ _ = pure GetPlayerCount

cGetPlayerCount
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cGetPlayerCount env =
  let
    gen _s = Just . pure $ GetPlayerCount
    exe _i = evalEither =<< successClient env (unPlayerCount <$> getPlayerCount)
  in
    Command gen exe [
      Ensure $ \(LeaderboardState ps as) _sNew _i c -> do
        annotateShow ps
        annotateShow as
        length ps === fromIntegral c
        assert $ length ps >= length as
    ]

--------------------------------------------------------------------------------
-- ME
--------------------------------------------------------------------------------

newtype Me (v :: * -> *) =
  Me (PlayerWithRsp v)
  deriving (Eq, Show)
instance HTraversable Me where
  htraverse f (Me pwr) = Me <$> htraverse f pwr

cMeGen
  :: MonadGen n
  => LeaderboardState Symbolic
  -> Maybe (n (Me Symbolic))
cMeGen (LeaderboardState ps _) =
  fmap Me <$> genPlayerWithRsp ps

cMeExecute
  :: ( MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Me Concrete
  -> m Player
cMeExecute env (Me pwr) =
  evalEither =<< successClient env (me (clientToken pwr))

cMeCallbacks
  :: [Callback Me Player LeaderboardState]
cMeCallbacks = [
    Require $ \(LeaderboardState ps _) (Me p) ->
      M.member (_pwrEmail p) ps
  , Ensure $
    \(LeaderboardState ps _) _ _ Player{..} -> do
      PlayerWithRsp{..} <- eval (ps M.! _playerEmail)
      let
        pwrAdmin = fromMaybe True _pwrIsAdmin
      _rspId (concrete _pwrRsp) === LS.PlayerId _playerId
      _pwrUsername === _playerUsername
      _pwrEmail === _playerEmail
      pwrAdmin === _playerIsAdmin
 ]

cMe
  :: ( MonadGen n
     , MonadTest m
     , MonadIO m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cMe env =
    Command cMeGen (cMeExecute env) cMeCallbacks

--------------------------------------------------------------------------------
-- REGISTER FIRST
--------------------------------------------------------------------------------

newtype RegFirst (v :: * -> *) =
    RegFirst RegisterPlayer
  deriving (Eq, Show)
instance HTraversable RegFirst where
  htraverse _ (RegFirst rp)          = pure (RegFirst rp)

newtype RegFirstForbidden (v :: * -> *) =
  RegFirstForbidden RegisterPlayer
  deriving (Eq, Show)
instance HTraversable RegFirstForbidden where
  htraverse _ (RegFirstForbidden rp) = pure (RegFirstForbidden rp)

cRegisterFirst
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cRegisterFirst env =
  let
    gen (LeaderboardState ps _as) =
      if null ps
      then Just $ RegFirst <$> genRegPlayerRandomAdmin ps
      else Nothing
    execute (RegFirst rp) =
       -- Force admin flag to true so our local state always aligns with DB
       evalEither =<< successClient env (registerFirst $ rp {_lbrIsAdmin = Just True})
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as) _input -> null ps
    , Update $
        \_ (RegFirst lbr@LeaderboardRegistration{..}) rsp ->
           let
             lbr' = lbr {_lbrIsAdmin = Just True}
             player = mkPlayerWithRsp lbr' rsp
             players = M.singleton _lbrEmail player
             admins = S.singleton _lbrEmail
           in
             LeaderboardState players admins
    ]

cRegisterFirstForbidden
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cRegisterFirstForbidden env =
  let
    gen (LeaderboardState ps _as) =
      bool (Just $ RegFirstForbidden <$> genRegPlayerRandomAdmin ps) Nothing $ null ps
    execute (RegFirstForbidden rp) =
      evalEither =<< failureClient env (registerFirst rp)
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as) _input -> not (null ps)
    , Ensure $ \sOld sNew _input se -> do
        sOld === sNew
        case se of
          FailureResponse{..} -> responseStatus === forbidden403
          _                   -> failure
    ]

--------------------------------------------------------------------------------
-- REGISTER
--------------------------------------------------------------------------------

data Register (v :: * -> *) =
  Register RegisterPlayer (PlayerWithRsp v)
  deriving (Eq, Show)

instance HTraversable Register where
  htraverse f (Register rp PlayerWithRsp{..}) =
    let mkFP (Var rsp) =
          fmap (\_pwrRsp -> PlayerWithRsp{..}) $ Var <$> f rsp
     in Register rp <$> mkFP _pwrRsp

cRegisterGen
  :: forall n.
     MonadGen n
  => LeaderboardState Symbolic
  -> Maybe (n (Register Symbolic))
cRegisterGen (LeaderboardState ps as) =
  if null as
  then Nothing
  else
    let
      maybeGenAdmin :: Maybe (n (PlayerWithRsp Symbolic))
      maybeGenAdmin =
        pure $ (M.!) ps <$> (Gen.element . S.toList $ as)
    in
      (Register <$> genRegPlayerRandomAdmin ps <*>)
        <$> maybeGenAdmin

cRegisterExecute
  :: ( MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Register Concrete
  -> m ResponsePlayer
cRegisterExecute env (Register rp p) =
  evalEither =<< successClient env (register (clientToken p) rp)

cRegisterCallbacks = [
    Require $ \(LeaderboardState ps _) (Register rp _) ->
      M.notMember (_lbrEmail rp) ps
  , Require $ \(LeaderboardState _ as) (Register _ p) ->
      S.member (_pwrEmail p) as
  , Update $
      \(LeaderboardState ps as)
       (Register rp@LeaderboardRegistration{..} _)
       rsp ->
         let
           newPlayers = M.insert _lbrEmail (mkPlayerWithRsp rp rsp) ps
           newAdmins =
             case _lbrIsAdmin of
               Just True -> S.insert _lbrEmail as
               _         -> as
         in
           LeaderboardState newPlayers newAdmins
  ]

cRegister
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cRegister env =
    Command cRegisterGen
            (cRegisterExecute env)
            cRegisterCallbacks

propRegisterMe
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegisterMe env reset =
  testProperty "register-me" . property $ do
  let
    initialState = LeaderboardState M.empty S.empty
    commands = [
        cRegisterFirst env
      , cRegisterFirstForbidden env
      , cRegister env
      , cGetPlayerCount env
      , cMe env
      ]
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands

  test $ do
    liftIO reset
    executeSequential initialState actions

