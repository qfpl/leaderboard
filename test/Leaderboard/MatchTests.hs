{-# LANGUAGE KindSignatures #-}

module Leaderboard.MatchTests
  ( matchTests
  ) where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Data.Time               (UTCTime)
import           Database.Beam           (Auto (..))
import           Servant.Client          (ClientEnv, ClientM, ServantError (..),
                                          runClientM)

import           Hedgehog                (Callback (..), Command (Command), Gen,
                                          HTraversable (htraverse), MonadTest,
                                          PropertyT, annotateShow,
                                          executeSequential, failure, forAll,
                                          property, (===))
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.Hedgehog     (testProperty)

import           Leaderboard.Schema      (PlayerId (..))
import           Leaderboard.SharedState (LeaderboardState (..))
import           Leaderboard.TestClient  (fromLbToken', getPlayerCount,
                                          register, registerFirst)
import           Leaderboard.Types       (RqMatch)

matchTests
  :: IO ()
  -> ClientEnv
  -> TestTree
matchTests resetDb env =
  testGroup "match" [
    propMatchTests env resetDb
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

cAddMatch = undefined
cListMatches = undefined

initialState
  :: LeaderboardState (v :: * -> *)
initialState =
  LeaderboardState M.empty S.empty M.empty

propMatchTests
  :: ClientEnv
  -> IO ()
  -> TestTree
propMatchTests env resetDb =
  testProperty "matches" . property $ do
  liftIO resetDb
  let cs = ($ env) <$> [cAddMatch, cListMatches]
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState cs
  executeSequential initialState commands
