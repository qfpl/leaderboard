{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  , cRegisterFirst
  , cRegister
  , cGetPlayerCount
  ) where

import           Control.Lens              (anyOf, at, each, set, (&), (^.))
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
import           Leaderboard.SharedState   (HasAdmins (admins), SeqOrPara (..),
                                            HasPlayers (players),
                                            LeaderboardState (..), PlayerMap,
                                            PlayerWithRsp (..), checkCommands, checkCommandsParallel,
                                            clientToken, email, emptyState,
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
    , propParallel env truncateTables
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
  :: forall m n state.
     ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasPlayers state
     , HasAdmins state
     )
  => SeqOrPara
  -> ClientEnv
  -> Command n m state
cGetPlayerCount sop env =
  let
    canRun s = sop == Sequential || (s ^. players & not . null)
    gen' = Just (pure GetPlayerCount)
    gen s = bool Nothing gen' $ canRun s
    exe _i = evalEither =<< successClient env (unPlayerCount <$> getPlayerCount)
  in
    Command gen exe [
      Require $ \s _i -> canRun s
    , Ensure $ \s _sNew _i c -> do
        length (s ^. players) === fromIntegral c
        assert $ length (s ^. admins) <= fromIntegral c
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
     , HasPlayers state
     , HasAdmins state
     )
  => ClientEnv
  -> Command n m state
cMe env =
  let
    gen state = (fmap . fmap) Me $ genPlayerWithRsp (state ^. players)
    exe (Me pwr) = evalEither =<< successClient env (me (clientToken pwr))
  in
    Command gen exe
    [ Require $ \s (Me PlayerWithRsp{..}) -> anyOf (players . each . email) (== _pwrEmail) s
    , Require $ \s _in -> not (null (s ^. admins))
    , Ensure $ \sOld _sNew _i p@Player{..} ->
        case sOld ^. players . at _playerEmail of
          Just (pwr@PlayerWithRsp{..}) -> do
          -- If there's only one user it should be an admin regardless of what we input
            let pwrAdmin = fromMaybe False _pwrIsAdmin
            annotateShow . length $ sOld ^. players
            annotateShow pwr
            annotateShow p
            (_rspId . concrete $ _pwrRsp) === LS.PlayerId _playerId
            _pwrUsername === _playerUsername
            _pwrEmail === _playerEmail
            pwrAdmin === _playerIsAdmin
          Nothing -> failure
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
     , HasPlayers state
     , HasAdmins state
     )
  => ClientEnv
  -> Command n m state
cRegisterFirst env =
  let
    gen state =
      if state ^. players & null
      then Just . fmap RegFirst . genRegPlayerRandomAdmin $ state ^. players
      else Nothing
    execute (RegFirst rp) =
       -- Force admin flag to true so our local state always aligns with DB
       evalEither =<< successClient env (registerFirst $ rp {_lbrIsAdmin = Just True})
  in
    Command gen execute [
      Require $ \state _input -> state ^. players & null
    , Update $ \state (RegFirst lbr@LeaderboardRegistration{..}) rsp ->
        let
          lbr' = lbr {_lbrIsAdmin = Just True}
          ps = M.singleton _lbrEmail (mkPlayerWithRsp lbr' rsp)
          as = S.singleton _lbrEmail
         in
          set players ps . set admins as $ state
    , Ensure $ \_sOld sNew (RegFirst _rp) _t -> (sNew ^. players & length) === 1
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
    ($ env) <$> [cRegisterFirst, cGetPlayerCount Sequential, cRegisterFirstForbidden]

propRegister
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegister env reset =
  checkCommands "register-all" reset emptyState $
    ($ env) <$> [cRegister, cRegisterFirst, cRegisterFirstForbidden, cGetPlayerCount Sequential, cMe]

propParallel
  :: ClientEnv
  -> IO ()
  -> TestTree
propParallel env reset =
  checkCommandsParallel "register-parallel" reset emptyState $
    ($ env) <$> [cRegister, cRegisterFirst, cRegisterFirstForbidden, cGetPlayerCount Parallel, cMe]

