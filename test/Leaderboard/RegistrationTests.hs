{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Control.Exception          (throw)
import           Control.Lens               ((&), (.~))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Morph        (hoist)
import           Control.Monad.Trans.Class  (lift)
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
                                             forAll, property, (===))
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Test.Tasty                 (TestName, TestTree, testGroup)
import           Test.Tasty.Hedgehog        (testProperty)

import           Leaderboard.TestClient     (LeaderboardClient (..),
                                             mkLeaderboardClient)
import           Leaderboard.Types          (ApplicationOptions (..),
                                             Login (..), PlayerCount (..),
                                             RegisterPlayer (..), dbConnInfo,
                                             _connectDatabase)

registrationTests
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTests truncateTables env =
    testGroup "registration" [
      propRegFirst env truncateTables
    , propRegister env truncateTables
    ]

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

data RegFirst (v :: * -> *) =
    RegFirst RegisterPlayer
  | RegFirstForbidden RegisterPlayer
  deriving (Eq, Show)
instance HTraversable RegFirst where
  htraverse _ (RegFirst rp)          = pure (RegFirst rp)
  htraverse _ (RegFirstForbidden rp) = pure (RegFirstForbidden rp)

getPlayer
  :: RegFirst v -> RegisterPlayer
getPlayer (RegFirst rp)          = rp
getPlayer (RegFirstForbidden rp) = rp

cRegFirst
  :: ClientEnv
  -> Command Gen (PropertyT IO) RegisterState
cRegFirst env =
  let
    gen (RegisterState n) =
      let f = bool RegFirstForbidden RegFirst (n == 0)
       in Just (f <$> genRegPlayerRandomAdmin)
    execute cmd =
      let
        reg = lcRegisterFirst mkLeaderboardClient . getPlayer $ cmd
        count = lcPlayerCount mkLeaderboardClient
      in
        lift . flip runClientM env $ (,) <$> reg <*> count
  in
    Command gen execute [
      Update $ \s c _out ->
        case c of
          RegFirst _          -> RegisterState 1
          RegFirstForbidden _ -> s
    , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input r ->
        case r of
          Right (_, PlayerCount c) ->
            sOld === 0
            >> sNew === 1
            >> c === 1
          Left e -> do
            annotateShow e
            case e of
              FailureResponse{..} ->
                assert (sOld > 0)
                >> sOld === sNew
                >> responseStatus === forbidden403
              _ -> failure
    ]

--------------------------------------------------------------------------------
-- REGISTER
--------------------------------------------------------------------------------

newtype Register (v :: * -> *) =
  Register RegisterPlayer
  deriving (Eq, Show)
instance HTraversable Register where
  htraverse _ (Register rp) = pure (Register rp)

cRegister
  :: Token
  -> Command Gen (PropertyT ClientM) RegisterState
cRegister token =
  let
    gen _s = Just (Register <$> genRegPlayerRandomAdmin)
    execute (Register rp) =
      let
        -- Empty token because client should manage cookie auth for us
        reg = lcRegister mkLeaderboardClient token rp
        count = lcPlayerCount mkLeaderboardClient
      in
        lift $ (,) <$> reg <*> count
  in
    Command gen execute [
      Update $ \(RegisterState n) c _out -> RegisterState (n + 1)
    , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input (_, PlayerCount c) ->
        sNew === sOld + 1
        >> c === sNew
    ]

propRegFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegFirst env truncateTables =
  testProperty "register-first" . property $ do
  liftIO truncateTables
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState [cRegFirst env]
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
    clientMToIO a = do
      ea <- runClientM a env
      either throw pure ea

  hoist clientMToIO $ do
    liftIO truncateTables
    lift $ lcRegisterFirst mkLeaderboardClient newPlayer
    token <- lift . lcLogin mkLeaderboardClient $ Login _lbrEmail _lbrPassword
    commands <- forAll $
      Gen.sequential (Range.linear 1 100) initialState [cRegister (Token token)]
    executeSequential initialState commands

