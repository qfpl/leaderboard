{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Bool                 (bool)
import qualified Data.Map                  as M
import           Data.Semigroup            ((<>))
import qualified Data.Set                  as S
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Auth.Client       (Token)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            Gen, HTraversable (htraverse),
                                            PropertyT, Var (Var), annotateShow,
                                            assert, concrete, executeSequential,
                                            failure, forAll, property, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import           Leaderboard.SharedState   (LeaderboardState (..), PlayerMap,
                                            PlayerWithToken (..), failureClient,
                                            genAdminToken, successClient)
import           Leaderboard.TestClient    (fromLbToken, fromLbToken',
                                            getPlayerCount, register,
                                            registerFirst)
import           Leaderboard.Types         (PlayerCount (..),
                                            RegisterPlayer (..))

registrationTests
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTests truncateTables env =
    testGroup "registration" [
      propRegFirst env truncateTables
    , propRegister env truncateTables
    ]

genRegPlayerRandomAdmin
  :: PlayerMap v
  -> Gen RegisterPlayer
genRegPlayerRandomAdmin ps =
  let
    genNonEmptyUnicode = Gen.text (Range.linear 1 100) Gen.unicode
  in
    LeaderboardRegistration
      <$> Gen.filter (`S.notMember` M.keysSet ps) genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> Gen.maybe Gen.bool

mkPlayerWithToken
  :: RegisterPlayer
  -> Var Token v
  -> PlayerWithToken v
mkPlayerWithToken LeaderboardRegistration{..} _pwtToken =
  let
    _pwtEmail = _lbrEmail
    _pwtName = _pwtPassword
    _pwtPassword = _lbrPassword
    _pwtIsAdmin = _lbrIsAdmin
  in
    PlayerWithToken{..}

--------------------------------------------------------------------------------
-- PLAYER COUNT
--------------------------------------------------------------------------------

data GetPlayerCount (v :: * -> *) =
    GetPlayerCount
  deriving (Eq, Show)
instance HTraversable GetPlayerCount where
  htraverse _ _ = pure GetPlayerCount

cGetPlayerCount
  :: ClientEnv
  -> Command Gen (PropertyT IO) LeaderboardState
cGetPlayerCount env =
  let
    gen _s = Just . pure $ GetPlayerCount
    exe _i = successClient show env $ unPlayerCount <$> getPlayerCount
  in
    Command gen exe [
      Ensure $ \(LeaderboardState ps as) _sNew _i c -> do
        annotateShow ps
        annotateShow as
        length ps === fromIntegral c
        assert $ length ps >= length as
    ]

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

cRegFirst
  :: ClientEnv
  -> Command Gen (PropertyT IO) LeaderboardState
cRegFirst env =
  let
    gen (LeaderboardState ps _as) =
      bool Nothing (Just $ RegFirst <$> genRegPlayerRandomAdmin ps) $ null ps
    execute (RegFirst rp) =
      let mkError = (("Should succeed with token, but got: " <>) . show)
       in successClient mkError env . fmap fromLbToken' $ registerFirst rp
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _) _input -> null ps
    , Update $ \_s (RegFirst lbr@LeaderboardRegistration{..}) t ->
        LeaderboardState (M.singleton _lbrEmail (mkPlayerWithToken lbr t)) (S.singleton _lbrEmail)
    , Ensure $ \_sOld (LeaderboardState psNew _) (RegFirst _rp) _t -> length psNew === 1
    ]

cRegFirstForbidden
  :: ClientEnv
  -> Command Gen (PropertyT IO) LeaderboardState
cRegFirstForbidden env =
  let
    gen (LeaderboardState ps _as) =
      bool (Just $ RegFirstForbidden <$> genRegPlayerRandomAdmin ps) Nothing $ null ps
    execute (RegFirstForbidden rp) =
      failureClient (const "Should return 403") env $ registerFirst rp
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
  Register RegisterPlayer (Var Token v)
  deriving (Eq, Show)
instance HTraversable Register where
  htraverse f (Register rp (Var gt)) = Register rp . Var <$> f gt

cRegister
  :: ClientEnv
  -> Command Gen (PropertyT IO) LeaderboardState
cRegister env =
  let
    gen rs@(LeaderboardState ps _as) =
      (Register <$> genRegPlayerRandomAdmin ps <*>) <$> genAdminToken rs
    execute (Register rp token) =
      successClient show env . fmap fromLbToken $ register (concrete token) rp
  in
    Command gen execute [
      Require $ \(LeaderboardState _ps as) _input -> not (null as)
    , Update $ \(LeaderboardState ps as) (Register rp@LeaderboardRegistration{..} _rqToken) t ->
        let
          newPlayers = M.insert _lbrEmail (mkPlayerWithToken rp t) ps
          newAdmins =
            case _lbrIsAdmin of
              Just True -> S.insert _lbrEmail as
              _         -> as
        in
          LeaderboardState newPlayers newAdmins
    , Ensure $ \(LeaderboardState psOld _asOld) (LeaderboardState psNew _asNew) _input _output ->
        length psNew === length psOld + 1
    ]

initialState
  :: LeaderboardState (v :: * -> *)
initialState =
  LeaderboardState M.empty S.empty

propRegFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegFirst env truncateTables =
  testProperty "register-first" . property $ do
  liftIO truncateTables
  let cs = ($ env) <$> [cRegFirst, cGetPlayerCount, cRegFirstForbidden]
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState cs
  executeSequential initialState commands

propRegister
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegister env truncateTables =
  testProperty "register-counts" . property $ do
  liftIO truncateTables
  let cs = ($ env) <$> [cRegister, cRegFirst, cRegFirstForbidden, cGetPlayerCount]
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState cs
  executeSequential initialState commands

