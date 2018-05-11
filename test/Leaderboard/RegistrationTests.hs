{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Text                 (Text)
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Auth.Client       (Token)
import           Servant.Client            (ClientEnv, ClientM,
                                            ServantError (..), runClientM)

import           Hedgehog                  (Callback (..), Command (Command),
                                            Gen, HTraversable (htraverse),
                                            MonadTest, PropertyT, annotateShow,
                                            executeSequential, failure, forAll,
                                            property, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import           Leaderboard.Schema        (RegisterPlayer (..))
import           Leaderboard.TestClient    (playerCount, register,
                                            registerFirst, toServantToken)
import           Leaderboard.Types         (PlayerCount (..))

registrationTests
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTests truncateTables env =
    testGroup "registration" [
      propRegFirst env truncateTables
    , propRegister env truncateTables
    ]

failureClient
  :: MonadIO m
  => (a -> String)
  -> ClientEnv
  -> ClientM a
  -> m ServantError
failureClient f ce a = do
  r <- liftIO $ runClientM a ce
  either pure (fail . f) r

successClient
  :: MonadIO m
  => (ServantError -> String)
  -> ClientEnv
  -> ClientM a
  -> m a
successClient f ce a = do
  r <- liftIO $ runClientM a ce
  either (fail . f) pure r

genNonEmptyUnicode
  :: Gen Text
genNonEmptyUnicode =
  Gen.text (Range.linear 1 100) Gen.unicode

genRegPlayerRandomAdmin
  :: Gen RegisterPlayer
genRegPlayerRandomAdmin =
  LeaderboardRegistration
    <$> genNonEmptyUnicode
    <*> genNonEmptyUnicode
    <*> genNonEmptyUnicode
    <*> (Just <$> Gen.bool)

--------------------------------------------------------------------------------
-- REGISTER FIRST
--------------------------------------------------------------------------------

newtype RegisterState (v :: * -> *) =
  RegisterState Integer
  deriving (Eq, Show)

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
  -> Command Gen (PropertyT IO) RegisterState
cRegFirst env =
  let
    gen _s =
       Just (RegFirst <$> genRegPlayerRandomAdmin)
    execute (RegFirst rp) =
      let
        reg = registerFirst rp
        pc = getPlayerCount <$> playerCount
      in
        successClient show env $ (,,) <$> pc <*> reg <*> pc
  in
    Command gen execute [
      Require $ \(RegisterState s) _input -> s == 0
    , Update $ \_s _c _out -> RegisterState 1
    , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input (cb, _r, ca) -> do
        annotateRegFirst sOld sNew cb ca
        sOld === 0
        sNew === 1
        cb === 0
        ca === 1
    ]

cRegFirstForbidden
  :: ClientEnv
  -> Command Gen (PropertyT IO) RegisterState
cRegFirstForbidden env =
  let
    gen _s = Just $ RegFirstForbidden <$> genRegPlayerRandomAdmin
    execute (RegFirstForbidden rp) =
      let
        reg = failureClient (const "Should return 403") env $ registerFirst rp
        pc = successClient show env $ getPlayerCount <$> playerCount
      in
        (,,) <$> pc <*> reg <*> pc
  in
    Command gen execute [
      Require $ \(RegisterState s) _input -> s > 0
    , Update $ \s _c _out -> s
    , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input (cb, se, ca) -> do
        annotateRegFirst sOld sNew cb ca
        sNew === sOld
        cb === sOld
        ca === sOld
        case se of
          FailureResponse{..} -> responseStatus === forbidden403
          _                   -> failure
    ]

annotateRegFirst
  :: ( Show a , MonadTest m)
  => a -> a -> a -> a -> m ()
annotateRegFirst sOld sNew cb ca = do
  annotateShow sOld
  annotateShow sNew
  annotateShow cb
  annotateShow ca


--------------------------------------------------------------------------------
-- REGISTER
--------------------------------------------------------------------------------

newtype Register (v :: * -> *) =
  Register RegisterPlayer
  deriving (Eq, Show)
instance HTraversable Register where
  htraverse _ (Register rp) = pure (Register rp)

cRegister
  :: ClientEnv
  -> Token
  -> Command Gen (PropertyT IO) RegisterState
cRegister env token =
  let
    gen _s = Just (Register <$> genRegPlayerRandomAdmin)
    execute (Register rp) =
      let pc = getPlayerCount <$> playerCount
       in successClient show env $ (,) <$> register token rp <*> pc
  in
    Command gen execute [
      Require $ \(RegisterState s) _input -> s > 0
    , Update $ \(RegisterState n) _cmd _out -> RegisterState (n + 1)
    , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input (_, c) -> do
        sNew === sOld + 1
        c === sNew
    ]

propRegFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegFirst env truncateTables =
  testProperty "register-first" . property $ do
  let initialState = RegisterState 0
  liftIO truncateTables
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState $ ($ env) <$> [cRegFirst, cRegFirstForbidden]
  executeSequential initialState commands

propRegister
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegister env truncateTables =
  testProperty "register-counts" . property $ do
  let
    _lbrEmail = "test@qfpl.io" -- not a real email address
    _lbrName = "test"
    _lbrPassword = "password"
    _lbrIsAdmin = Just True
    newPlayer = LeaderboardRegistration{..}
    initialState = RegisterState 1

  liftIO truncateTables
  token <- fmap toServantToken . successClient show env $ registerFirst newPlayer
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState [cRegister env token, cRegFirst env, cRegFirstForbidden env]
  executeSequential initialState commands

