{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Control.Lens               ((&), (.~))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
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
                                             executeSequential, failure, forAll,
                                             property, (===))
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Test.Tasty                 (TestName, TestTree, testGroup)
import           Test.Tasty.Hedgehog        (testProperty)

import           Leaderboard.TestClient     (LeaderboardClient (..),
                                             mkLeaderboardClient)
--import           Leaderboard.TestServer     (truncateTables)
import           Leaderboard.Types          (ApplicationOptions (..),
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

data RegisterState (v :: * -> *) =
  RegisterState
  { registeredFirst :: Bool
  , userCount :: Int
  }
  deriving (Eq, Show)

initialState :: RegisterState v
initialState = RegisterState False 0

newtype RegFirst (v :: * -> *) =
  RegFirst RegisterPlayer
  deriving (Eq, Show)

instance HTraversable RegFirst where
  htraverse _ (RegFirst rp) = pure (RegFirst rp)

cRegFirst
  :: ClientEnv
  -> Command Gen (PropertyT IO) RegisterState
cRegFirst env =
  let
    gen = const . Just . fmap RegFirst $ genRegPlayerRandomAdmin
    execute (RegFirst rp) = lift . flip runClientM env $ lcRegisterFirst mkLeaderboardClient rp
  in
    Command gen execute [
      Update $ \_in _c _out -> RegFirstState True
    , Ensure $ \(RegisterState sOld) (RegisterState sNew) _input r ->
        case r of
          Right _ -> sOld === False >> sNew === True
          Left FailureResponse{..} ->
            sOld === True
            >> sNew === True
            >> responseStatus === forbidden403
          Left e -> annotateShow e >> failure
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
