module Leaderboard.MatchTests
  ( matchTests
  ) where

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

import           Leaderboard.TestClient    (playerCount, register,
                                            registerFirst, toServantToken)

matchTests
  :: IO ()
  -> ClientEnv
  -> TestTree
matchTests =
  testGroup "match" [
    
  ]

