module Leaderboard.MatchTests
  ( matchTests
  ) where

import           Data.Time              (UTCTime)
import           Database.Beam          (Auto (..))
import           Servant.Client         (ClientEnv, ClientM, ServantError (..),
                                         runClientM)

import           Hedgehog               (Callback (..), Command (Command), Gen,
                                         HTraversable (htraverse), MonadTest,
                                         PropertyT, annotateShow,
                                         executeSequential, failure, forAll,
                                         property, (===))
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.Hedgehog    (testProperty)

import           Leaderboard.Schema     (PlayerId (..))
import           Leaderboard.TestClient (getPlayerCount, register, registerFirst,
                                         fromLbToken')
import           Leaderboard.Types      (RqMatch)

matchTests
  :: IO ()
  -> ClientEnv
  -> TestTree
matchTests truncateTables env =
  testGroup "match" [

  ]

genPlayerId
  :: Gen PlayerId
genPlayerId =
  undefined
  -- PlayerId . Auto . Just <$> Gen.int (Range.linear 1 100)

genTimeStamp
  :: Gen UTCTime
genTimeStamp = undefined

genMatch
  :: Gen RqMatch
genMatch =
  undefined
  -- RqMatch
  -- <$> genPlayerId
  -- <*> genPlayerId
  -- <*> Gen.int (Range.linear 1 100)
  -- <*> Gen.int (Range.linear 1 100)
  -- <*> undefined
