module Main where

import           Test.Tasty                    (TestTree, defaultMain,
                                                testGroup)

import           Leaderboard.RegistrationTests (registrationTests)

main :: IO ()
main = defaultMain allTheTests

allTheTests :: TestTree
allTheTests =
  testGroup "leaderboard"
  [ registrationTests
  ]
