{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Control.Exception          (throw)
import           Control.Lens               ((&), (.~))
import           Control.Monad              ((<=<))
import           Control.Monad.IO.Class     (liftIO, MonadIO)
import           Control.Monad.Morph        (hoist)
import           Control.Monad.Trans.Class  (lift)
import           Data.Bifunctor             (bimap)
import           Data.Bool                  (bool)
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (ConnectInfo (..))
import           Network.HTTP.Types.Status  (forbidden403)
import           Servant.Auth.Client        (Token (Token))
import           Servant.Client             (BaseUrl (BaseUrl),
                                             ClientEnv (ClientEnv), ClientM,
                                             Scheme (Https), ServantError (..),
                                             runClientM)

import           Hedgehog                   (Callback (..), Command (Command),
                                             Gen, HTraversable (htraverse),
                                             Property, PropertyT, annotateShow,
                                             assert, executeSequential, failure,
                                             forAll, property, success, (===), annotate, MonadTest)
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Test.Tasty                 (TestName, TestTree, testGroup)
import           Test.Tasty.Hedgehog        (testProperty)

import           Leaderboard.API.Player     (AuthHeaders)
import           Leaderboard.TestClient     (authenticate, playerCount,
                                             register, registerFirst,
                                             toServantToken)
import           Leaderboard.Types          (ApplicationOptions (..),
                                             Login (..), PlayerCount (..),
                                             RegisterPlayer (..), dbConnInfo,
                                             _connectDatabase)
import qualified Leaderboard.Types          as Lb (Token)

registrationTests
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTests truncateTables env =
    testGroup "registration" [
      propRegFirst env truncateTables
    -- , propRegister env truncateTables
    ]

failureClient
  :: ( MonadIO m
     , MonadTest m
     )
  => (a -> String)
  -> ClientEnv
  -> ClientM a
  -> m ServantError
failureClient f ce a = do
  r <- liftIO $ runClientM a ce
  either pure (fail . f) r

successClient
  :: ( MonadIO m
     , MonadTest m
     )
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

initialState :: RegisterState v
initialState = RegisterState 0

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
    , Update $ \s c _out -> RegisterState 1
    , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input (cb, r, ca) -> do
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
        reg = registerFirst rp
        pc = successClient show env $ getPlayerCount <$> playerCount
      in
        (,,) <$> pc <*> failureClient (const "Should return 403") env reg <*> pc
  in
    Command gen execute [
      Require $ \(RegisterState s) _input -> s > 0
    , Update $ \s c _out -> s
    , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input (cb, se, ca) -> do
        annotateRegFirst sOld sNew cb ca
        sOld === 1
        sNew === 1
        cb === 1
        ca === 1
    ]

annotateRegFirst sOld sNew cb ca = do
  annotateShow sOld
  annotateShow sNew
  annotateShow cb
  annotateShow ca


--------------------------------------------------------------------------------
-- REGISTER
--------------------------------------------------------------------------------

-- newtype Register (v :: * -> *) =
--   Register RegisterPlayer
--   deriving (Eq, Show)
-- instance HTraversable Register where
--   htraverse _ (Register rp) = pure (Register rp)

-- cRegister
--   :: Token
--   -> Command Gen (PropertyT ClientM) RegisterState
-- cRegister token =
--   let
--     gen _s = Just (Register <$> genRegPlayerRandomAdmin)
--     execute (Register rp) =
--       let
--         -- Empty token because client should manage cookie auth for us
--         reg = register token rp
--       in
--         lift $ (,) <$> reg <*> playerCount
--   in
--     Command gen execute [
--       Update $ \(RegisterState n) c _out -> RegisterState (n + 1)
--     , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input (_, PlayerCount c) ->
--         sNew === sOld + 1
--         >> c === sNew
--     ]

propRegFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegFirst env truncateTables =
  testProperty "register-first" . property $ do
  liftIO truncateTables
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState $ ($ env) <$> [cRegFirst, cRegFirstForbidden]
  executeSequential initialState commands

-- propRegister
--   :: ClientEnv
--   -> IO ()
--   -> TestTree
-- propRegister env truncateTables =
--   testProperty "register-counts" . property $ do
--   let
--     _lbrEmail = "test@qfpl.io" -- not a real email address
--     _lbrName = "test"
--     _lbrPassword = "password"
--     _lbrIsAdmin = Just True
--     newPlayer = LeaderboardRegistration{..}
--     clientMToIO a = do
--       ea <- runClientM a env
--       either throw pure ea

--   hoist clientMToIO $ do
--     liftIO truncateTables
--     lift $ registerFirst newPlayer
--     token <- lift . fmap toServantToken . authenticate $ Login _lbrEmail _lbrPassword
--     commands <- forAll $
--       Gen.sequential (Range.linear 1 100) initialState [cRegister token]
--     executeSequential initialState commands

