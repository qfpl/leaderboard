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

import           Leaderboard.Gens          (genAdminWithRsp, genPlayerWithRsp,
                                            genRegPlayer,
                                            genRegPlayerUniqueEmail)
import           Leaderboard.Schema        (PlayerT (..))
import qualified Leaderboard.Schema        as LS
import           Leaderboard.SharedState   (CanRegisterPlayers (..),
                                            HasAdmins (admins),
                                            HasPlayerCount (playerCount),
                                            HasPlayers (players),
                                            PlayerWithRsp (..),
                                            RegFirstState (..), SeqOrPara (..),
                                            TestRsp (TestRsp), checkCommands,
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
    -- , propRegister env truncateTables
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
  -> Command n m RegFirstState
cGetPlayerCount env =
  let
    gen _state =
      Just (pure GetPlayerCount)
    exe _i =
      evalEither =<< successClient env (unPlayerCount <$> getPlayerCount)
  in
    Command gen exe [
      Ensure $ \(RegFirstState n) _sNew _i c ->
        n === fromIntegral c
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
  -> Command n m RegFirstState
cRegisterFirst env =
  let
    gen (RegFirstState n) =
      if n == 0
      then Just . fmap RegFirst $ genRegPlayer
      else Nothing
    execute (RegFirst rp) =
       fmap TestRsp $ evalEither =<< successClient env (registerFirst rp)
  in
    Command gen execute [
      Require $ \(RegFirstState n) _input -> n == 0
    , Update $ \_state _input _out ->
        RegFirstState 1
    ]

cRegisterFirstForbidden
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegFirstState
cRegisterFirstForbidden env =
  let
    gen (RegFirstState n) =
      if n == 0
      then Nothing
      else Just . fmap RegFirst $ genRegPlayer
    execute (RegFirst rp) =
      evalEither =<< failureClient env (registerFirst rp)
  in
    Command gen execute [
      Require $ \(RegFirstState n) _input -> (n > 0)
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
    Register rp <$> (Var <$> f rsp)

cRegister
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasAdmins state
     , HasPlayers state
     , HasPlayerCount state
     , CanRegisterPlayers state
     )
  => ClientEnv
  -> Command n m state
cRegister env =
  let
    gen state =
      if state ^. admins & null
      then Nothing
      else (Register <$> genRegPlayerUniqueEmail state <*>) <$> genAdminWithRsp state
    execute (Register rp rsp) =
      fmap TestRsp $ evalEither =<< successClient env (register (clientToken rsp) rp)
  in
    Command gen execute [
      Require $ \state (Register rp p) ->
           (state ^. players & M.notMember (_lbrEmail rp))
        && (state ^. admins & S.member p)
    , Update $ \state (Register rp _rqToken) rsp ->
        registerPlayer state rp rsp
    , Ensure $ \sOld sNew (Register LeaderboardRegistration{..} _t) rsp -> do
        sNew ^. players . at _lbrEmail & assert . is _Just
        sOld ^. players . at _lbrEmail & assert . is _Nothing
        sNew ^. playerCount === (sOld ^. playerCount & succ)
        let vRsp = Var (Concrete rsp)
        case _lbrIsAdmin of
          Just True -> do
            sNew ^. admins . at vRsp & assert . is _Just
            sOld ^. admins . at vRsp & assert . is _Nothing
            sNew ^. admins . to length === (sOld ^. admins . to length & succ)
          _ -> success
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
      RegFirstState 0
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands
  evalIO reset
  executeSequential initialState actions

-- propRegister
--   :: ClientEnv
--   -> IO ()
--   -> TestTree
-- propRegister env reset =
--   let
--     cs = ($ env) <$>
--       [ cRegister
--       , cRegisterFirst genRegPlayerUniqueEmail
--       , cRegisterFirstForbidden genRegPlayerUniqueEmail
--       , cGetPlayerCount Sequential
--       , cMe
--       ]
--   in
--     checkCommands "register-all" reset emptyState cs

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
