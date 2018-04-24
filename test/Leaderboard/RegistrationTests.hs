module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Hedgehog            (Callback (..), Command (Command), Gen,
                                      HTraversable (htraverse), Property,
                                      PropertyT, executeSequential, forAll,
                                      property, (===))
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Tasty.Hedgehog (testProperty)

newtype RegFirstState (v :: * -> *) =
  RegFirstState Bool
  deriving (Eq, Show)

registrationTests :: TestTree
registrationTests =
  testGroup "registration" [
    registerFirstTests
  ]
