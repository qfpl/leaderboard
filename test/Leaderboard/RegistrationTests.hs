{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Control.Lens               ((&), (.~))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Data.Bool                  (bool)
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (ConnectInfo (..))
import           Network.HTTP.Types.Status  (forbidden403)
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
                                             PlayerCount (..),
                                             RegisterPlayer (..), dbConnInfo,
                                             _connectDatabase)

registrationTests
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTests truncateTables env =
    testGroup "registration" [
      propRegFirst env truncateTables
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
