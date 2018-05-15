{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  , cRegisterFirst
  , cRegister
  , cGetPlayerCount
  ) where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Bool                 (bool)
import qualified Data.Map                  as M
import           Data.Semigroup            ((<>))
import qualified Data.Set                  as S
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            HTraversable (htraverse),
                                            PropertyT, Var (Var), annotateShow,
                                            assert, executeSequential, failure,
                                            forAll, property, success, (===), MonadGen, GenT, test)
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import           Leaderboard.SharedState   (LeaderboardState (..), PlayerMap, emptyState,
                                            PlayerWithRsp (..), clientToken,
                                            failureClient, genAdminWithRsp,
                                            successClient, checkCommands)
import           Leaderboard.TestClient    (getPlayerCount, register,
                                            registerFirst)
import           Leaderboard.Types         (PlayerCount (..),
                                            RegisterPlayer (..), RspPlayer (..))

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
  :: MonadGen n
  => PlayerMap v
  -> n RegisterPlayer
genRegPlayerRandomAdmin ps =
  let
    genNonEmptyUnicode = Gen.text (Range.linear 1 100) Gen.unicode
  in
    LeaderboardRegistration
      <$> Gen.filter (`S.notMember` M.keysSet ps) genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> Gen.maybe Gen.bool

mkPlayerWithRsp
  :: RegisterPlayer
  -> Var RspPlayer v
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
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) LeaderboardState
cGetPlayerCount env =
  let
    gen _s = Just . pure $ GetPlayerCount
    exe _i = successClient show env $ unPlayerCount <$> getPlayerCount
  in
    Command gen exe [
      Ensure $ \(LeaderboardState ps as _ms) _sNew _i c -> do
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

cRegisterFirst
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) LeaderboardState
cRegisterFirst env =
  let
    gen (LeaderboardState ps _as _ms) =
      if null ps
      then Just $ RegFirst <$> genRegPlayerRandomAdmin ps
      else Nothing
    execute (RegFirst rp) =
      let mkError = (("Should succeed with token, but got: " <>) . show)
       in successClient mkError env $ registerFirst rp
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as _ms) _input -> null ps
    , Update $ \(LeaderboardState _ps _as ms) (RegFirst lbr@LeaderboardRegistration{..}) rsp ->
        LeaderboardState (M.singleton _lbrEmail (mkPlayerWithRsp lbr rsp)) (S.singleton _lbrEmail) ms
    , Ensure $ \_sOld (LeaderboardState psNew _as _ms) (RegFirst _rp) _t -> length psNew === 1
    ]

cRegisterFirstForbidden
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) LeaderboardState
cRegisterFirstForbidden env =
  let
    gen (LeaderboardState ps _as _ms) =
      bool (Just $ RegFirstForbidden <$> genRegPlayerRandomAdmin ps) Nothing $ null ps
    execute (RegFirstForbidden rp) =
      failureClient (const "Should return 403") env $ registerFirst rp
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as _ms) _input -> not (null ps)
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
    let mkFP (Var rsp) = fmap (\_pwrRsp -> PlayerWithRsp{..}) $ Var <$> f rsp
     in Register rp <$> mkFP _pwrRsp

cRegister
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) LeaderboardState
cRegister env =
  let
    gen rs@(LeaderboardState ps as _ms) =
      if null as
      then Nothing
      else (Register <$> genRegPlayerRandomAdmin ps <*>) <$> genAdminWithRsp rs
    execute (Register rp p) =
      successClient show env $ register (clientToken p) rp
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as _ms) (Register rp _p) ->
        M.notMember (_lbrEmail rp) ps
    , Require $ \(LeaderboardState _ps as _ms) (Register _rp p) ->
        S.member (_pwrEmail p) as
    , Update $ \(LeaderboardState ps as ms) (Register rp@LeaderboardRegistration{..} _rqToken) rsp ->
        let
          newPlayers = M.insert _lbrEmail (mkPlayerWithRsp rp rsp) ps
          newAdmins =
            case _lbrIsAdmin of
              Just True -> S.insert _lbrEmail as
              _         -> as
        in
          LeaderboardState newPlayers newAdmins ms
    , Ensure $ \(LeaderboardState psOld asOld _msOld)
                (LeaderboardState psNew asNew _msNew)
                (Register LeaderboardRegistration{..} _t)
                _output -> do
        assert $ M.member _lbrEmail psNew
        assert $ M.notMember _lbrEmail psOld
        length psNew === length psOld + 1
        case _lbrIsAdmin of
          Just True -> do
            assert $ S.member _lbrEmail asNew
            assert $ S.notMember _lbrEmail asOld
            length asNew === length asOld + 1
          _ -> success
    ]

propRegFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegFirst env reset =
  checkCommands "register-first" reset emptyState $
    ($ env) <$> [cRegisterFirst, cGetPlayerCount, cRegisterFirstForbidden]

propRegister
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegister env reset =
  checkCommands "register-all" reset emptyState $
    ($ env) <$> [cRegister, cRegisterFirst, cRegisterFirstForbidden, cGetPlayerCount]

