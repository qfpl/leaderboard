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
  , propRegFirst
  ) where

import           Control.Lens              (anyOf, at, each, to, (&), (^.),
                                            _Just, _Nothing)
import           Control.Lens.Extras       (is)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Bool                 (bool)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as S
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            Concrete (Concrete),
                                            HTraversable (htraverse), MonadGen,
                                            MonadTest, Symbolic, Var (Var),
                                            annotateShow, assert, evalEither,
                                            evalIO, executeSequential, failure,
                                            forAll, property, success, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import           Leaderboard.Gens          (genAdminRsp, genPlayerWithRsp,
                                            genRegPlayer,
                                            genRegPlayerUniqueEmail)
import           Leaderboard.Schema        (PlayerT (..))
import qualified Leaderboard.Schema        as LS
import           Leaderboard.SharedState   (CanRegisterPlayers (..),
                                            HasAdmins (admins),
                                            HasPlayerCount (playerCount),
                                            HasPlayers (players),
                                            PlayerWithRsp (..),
                                            RegFirstState (..),
                                            RegisterState (RegisterState),
                                            SeqOrPara (..), TestRsp (TestRsp),
                                            checkCommands,
                                            checkCommandsParallel, clientToken,
                                            emptyState, failureClient, pwrEmail,
                                            pwrUsername, successClient)
import           Leaderboard.TestAPI       (PlayerCount (..))
import           Leaderboard.TestClient    (getPlayerCount, me, register,
                                            registerFirst)
import           Leaderboard.Types         (HasResponsePlayer (..),
                                            RegisterPlayer (..))

registrationTests
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTests truncateTables env =
    testGroup "registration" [
      propRegFirst env truncateTables
    , propRegister env truncateTables
    -- , propParallel env truncateTables
    ]

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
  -> Command n m RegisterState
cGetPlayerCount env =
  let
    gen _state =
      Just (pure GetPlayerCount)
    exe _i =
      evalEither =<< successClient env (unPlayerCount <$> getPlayerCount)
  in
    Command gen exe [
      Ensure $ \(RegisterState ps _) _sNew _i c ->
        length ps === fromIntegral c
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
    [ Require $ \s (Me PlayerWithRsp{..}) -> anyOf (players . each . pwrEmail) (== _pwrEmail) s
    , Require $ \s _in -> not (null (s ^. admins))
    , Ensure $ \sOld _sNew _i p@Player{..} ->
        case sOld ^. players . at _playerEmail of
          Just (pwr@PlayerWithRsp{..}) -> do
          -- If there's only one user it should be an admin regardless of what we input
            let pwrAdmin = fromMaybe False _pwrIsAdmin
            annotateShow . length $ sOld ^. players
            annotateShow pwr
            annotateShow p
            pwr ^. rspId & (=== LS.PlayerId _playerId)
            pwr ^. pwrUsername === _playerUsername
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

cRegisterFirst
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegisterState
cRegisterFirst env =
  let
    gen (RegisterState ps _) =
      if null ps
      then Just . fmap RegFirst $ genRegPlayer
      else Nothing
    execute (RegFirst rp) =
       fmap TestRsp $ evalEither =<< successClient env (registerFirst rp)
  in
    Command gen execute [
      Require $ \(RegisterState ps _) _input -> null ps
    , Update $ \_state (RegFirst rp) rsp ->
        RegisterState (S.singleton (_lbrEmail rp)) (S.singleton rsp)
    ]

cRegisterFirstForbidden
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegisterState
cRegisterFirstForbidden env =
  let
    gen (RegisterState ps _) =
      if null ps
      then Nothing
      else Just . fmap RegFirst $ genRegPlayer
    execute (RegFirst rp) =
      evalEither =<< failureClient env (registerFirst rp)
  in
    Command gen execute [
      Require $ \(RegisterState ps _) _input -> not (null ps)
    , Ensure $ \_sOld _sNew _input se ->
        case se of
          FailureResponse{..} -> responseStatus === forbidden403
          _                   -> failure
    ]

--------------------------------------------------------------------------------
-- REGISTER
--------------------------------------------------------------------------------

data Register (v :: * -> *) =
  Register RegisterPlayer (Var TestRsp v)
  deriving (Eq, Show)

instance HTraversable Register where
  htraverse f (Register rp (Var rsp)) =
    Register rp . Var <$> f rsp

cRegister
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegisterState
cRegister env =
  let
    gen (RegisterState ps as) =
      if null as
      then Nothing
      else (Register <$> genRegPlayerUniqueEmail ps <*>) <$> genAdminRsp as
    execute (Register rp rsp) =
      fmap TestRsp $ evalEither =<< successClient env (register (clientToken rsp) rp)
  in
    Command gen execute [
      Require $ \(RegisterState ps as) (Register rp p) ->
           S.notMember (_lbrEmail rp) ps
        && S.member p as
    , Update $ \(RegisterState ps as) (Register rp _rqToken) rsp ->
        let
          newPs = S.insert (_lbrEmail rp) ps
          newAs = bool as (S.insert rsp as) (_lbrIsAdmin rp == Just True)
        in
          RegisterState newPs newAs
    , Ensure $ \(RegisterState psOld asOld) (RegisterState psNew asNew)
          (Register LeaderboardRegistration{..} _t) rsp -> do
        assert $ S.member _lbrEmail psNew
        assert $ S.notMember _lbrEmail psOld
        length psNew === length psOld + 1
        let vRsp = Var (Concrete rsp)
        if _lbrIsAdmin == Just True
          then do
            assert (S.member vRsp asNew)
            assert (S.notMember vRsp asOld)
            length asNew === length asOld + 1
          else
            success
    ]

propRegFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegFirst env reset =
  testProperty "register-first" . property $ do
  let
    commands =
      ($ env) <$> [cRegisterFirst, cGetPlayerCount, cRegisterFirstForbidden]
    initialState =
      RegisterState S.empty S.empty
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands
  evalIO reset
  executeSequential initialState actions

propRegister
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegister env reset =
  testProperty "register-all" . property $ do
  let
    commands = ($ env) <$>
      [ cRegister
      , cRegisterFirst
      , cRegisterFirstForbidden
      , cGetPlayerCount
      ]
    initialState =
      RegisterState S.empty S.empty
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands
  evalIO reset
  executeSequential initialState actions

-- propParallel
--   :: ClientEnv
--   -> IO ()
--   -> TestTree
-- propParallel env reset =
--   let
--     cs = ($ env) <$>
--       [ cRegister
--       , cRegisterFirst genRegPlayerUniqueEmail
--       , cRegisterFirstForbidden genRegPlayerUniqueEmail
--       , cGetPlayerCount Parallel
--       , cMe
--       ]
--   in
--     checkCommandsParallel "register-parallel" reset emptyState cs
