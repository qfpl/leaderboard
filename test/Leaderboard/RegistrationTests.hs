{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  , cRegisterFirst
  , cRegister
  , cGetPlayerCount
  ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Bool                 (bool)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as S
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            HTraversable (htraverse), MonadGen,
                                            MonadTest, Var (Var), annotateShow,
                                            assert, concrete, evalEither,
                                            failure, success, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)

import           Leaderboard.Schema        (PlayerT (..))
import qualified Leaderboard.Schema        as LS
import           Leaderboard.SharedState   (LeaderboardState (..), PlayerMap,
                                            PlayerWithRsp (..), checkCommands,
                                            clientToken, emptyState,
                                            failureClient, genAdminWithRsp,
                                            genPlayerWithRsp, successClient)
import           Leaderboard.TestClient    (getPlayerCount, me, register,
                                            registerFirst)
import           Leaderboard.Types         (PlayerCount (..),
                                            RegisterPlayer (..),
                                            ResponsePlayer (..))

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
    genNonEmptyUnicode = Gen.text (Range.linear 1 20) Gen.alphaNum
  in
    LeaderboardRegistration
      <$> Gen.filter (`S.notMember` M.keysSet ps) genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> Gen.maybe Gen.bool

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
      Ensure $ \(LeaderboardState ps as _ms) _sNew _i c -> do
        annotateShow ps
        annotateShow as
        length ps === fromIntegral c
        assert $ length ps >= length as
    ]

newtype Me (v :: * -> *) =
  Me (PlayerWithRsp v)
  deriving (Eq, Show)
instance HTraversable Me where
  htraverse f (Me pwr) = Me <$> htraverse f pwr

cMe
  :: ( MonadGen n
     , MonadTest m
     , MonadIO m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cMe env =
  let
    gen (LeaderboardState ps _as _ms) = bool (fmap Me <$> genPlayerWithRsp ps) Nothing $ null ps
    exe (Me pwr) = evalEither =<< successClient env (me (clientToken pwr))
  in
    Command gen exe
    [ Require $ \(LeaderboardState ps _as _ms) (Me PlayerWithRsp{..}) -> M.member _pwrEmail ps
    , Require $ \(LeaderboardState _ps as _ms) _in -> not (null as)
    , Ensure $ \(LeaderboardState ps _as _ms) _sNew _i p@Player{..} -> do
        let
          pwr@PlayerWithRsp{..} = ps M.! _playerEmail
          -- If there's only one user it should be an admin regardless of what we input
          pwrAdmin = fromMaybe False _pwrIsAdmin
        annotateShow $ length ps
        annotateShow pwr
        annotateShow p
        (_rspId . concrete $ _pwrRsp) === LS.PlayerId _playerId
        _pwrUsername === _playerUsername
        _pwrEmail === _playerEmail
        pwrAdmin === _playerIsAdmin
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
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cRegisterFirst env =
  let
    gen (LeaderboardState ps _as _ms) =
      if null ps
      then Just $ RegFirst <$> genRegPlayerRandomAdmin ps
      else Nothing
    execute (RegFirst rp) =
       -- Force admin flag to true so our local state always aligns with DB
       evalEither =<< successClient env (registerFirst $ rp {_lbrIsAdmin = Just True})
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as _ms) _input -> null ps
    , Update $ \(LeaderboardState _ps _as ms) (RegFirst lbr@LeaderboardRegistration{..}) rsp ->
        let lbr' = lbr {_lbrIsAdmin = Just True}
         in LeaderboardState (M.singleton _lbrEmail (mkPlayerWithRsp lbr' rsp)) (S.singleton _lbrEmail) ms
    , Ensure $ \_sOld (LeaderboardState psNew _as _ms) (RegFirst _rp) _t -> length psNew === 1
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
    gen (LeaderboardState ps _as _ms) =
      bool (Just $ RegFirstForbidden <$> genRegPlayerRandomAdmin ps) Nothing $ null ps
    execute (RegFirstForbidden rp) =
      evalEither =<< failureClient env (registerFirst rp)
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
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cRegister env =
  let
    gen rs@(LeaderboardState ps as _ms) =
      if null as
      then Nothing
      else (Register <$> genRegPlayerRandomAdmin ps <*>) <$> genAdminWithRsp rs
    execute (Register rp p) =
      evalEither =<< successClient env (register (clientToken p) rp)
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
    ($ env) <$> [cRegister, cRegisterFirst, cRegisterFirstForbidden, cGetPlayerCount, cMe]

