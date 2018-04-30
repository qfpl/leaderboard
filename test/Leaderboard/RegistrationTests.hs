{-# LANGUAGE KindSignatures #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Data.Text           (Text)
import           Hedgehog            (Callback (..), Command (Command), Gen,
                                      HTraversable (htraverse), Property,
                                      PropertyT, executeSequential, forAll,
                                      property, (===))
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Leaderboard.Types   (RegisterPlayer (..))

registrationTests :: TestTree
registrationTests =
  testGroup "registration" [
    -- registerFirstTests
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

newtype RegFirstState (v :: * -> *) =
  RegFirstState Bool
  deriving (Eq, Show)

initialState :: RegFirstState v
initialState = RegFirstState False

newtype RegFirst (v :: * -> *) =
  RegFirst RegisterPlayer
  deriving (Eq, Show)

instance HTraversable RegFirst where
  htraverse _ (RegFirst rp) = pure (RegFirst rp)

cRegFirst
  :: Command Gen (PropertyT IO) RegFirstState
cRegFirst =
  let
    gen :: RegFirstState v -> Maybe (Gen (RegFirst v))
    gen = const . Just . fmap RegFirst $ genRegPlayerRandomAdmin
  in
    undefined
