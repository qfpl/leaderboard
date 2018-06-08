module Leaderboard.JsonTests where

import Hedgehog (property, (===), forAll)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Aeson (encode, decode)

import Leaderboard.Gens (genRqMatch)

jsonTests :: TestTree
jsonTests =
  testGroup "json" [
    propRqMatch
  ]

propRqMatch :: TestTree
propRqMatch =
  testProperty "RqMatch round trip" . property $ do
    m <- forAll genRqMatch
    decode (encode m) === Just m
