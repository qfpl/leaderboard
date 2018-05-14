{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Leaderboard.MatchTests
  ( matchTests
  ) where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Data.Time               (UTCTime (UTCTime), fromGregorian,
                                          secondsToDiffTime)
import           Data.Traversable        (sequenceA)
import           Database.Beam           (Auto (..))
import           Servant.Auth.Client     (Token)
import           Servant.Client          (ClientEnv, ClientM, ServantError (..),
                                          runClientM)

import           Hedgehog                (Callback (..), Command (Command), Gen,
                                          HTraversable (htraverse), MonadGen,
                                          MonadTest, PropertyT, Var,
                                          annotateShow, executeSequential,
                                          failure, forAll, property, (===))
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.Hedgehog     (testProperty)

import           Leaderboard.Schema      (PlayerId (..))
import           Leaderboard.SharedState (LeaderboardState (..), PlayerMap,
                                          genPlayerToken)
import           Leaderboard.TestClient  (fromLbToken', getPlayerCount,
                                          register, registerFirst)
import           Leaderboard.Types       (RqMatch (RqMatch))

matchTests
  :: IO ()
  -> ClientEnv
  -> TestTree
matchTests resetDb env =
  testGroup "match" [
    propMatchTests env resetDb
  ]

genPlayerId
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n PlayerId)
genPlayerId =
  undefined
  -- PlayerId . Auto . Just <$> Gen.int (Range.linear 1 100)

genTwoPlayerIds
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n (PlayerId, PlayerId))
genTwoPlayerIds ps =
  if length ps < 2
  then Nothing
  else Just $ do
    -- Beware the Gen.just here. As long as we've checked we have enough players to satisfy generating
    -- the ids we need, then this should be fine.
    let genPlayerIdJust = Gen.just . sequenceA . genPlayerId $ ps
    p1Id <- genPlayerIdJust
    p2Id <- Gen.filter (/= p1Id) genPlayerIdJust
    pure (p1Id, p2Id)

genTimeStamp
  :: MonadGen n
  => n UTCTime
genTimeStamp =
  let
    gYear = Gen.int (Range.linearFrom 2000 1970 2100)
    gMonth = Gen.int (Range.linear 1 12)
    -- fromGregorian automatically trims to valid dates, so 2001-02-31 becomes 2001-02-28
    gDay = Gen.int (Range.linear 1 31)
    hToS = (* 3600)
    gSeconds = Gen.int (Range.linearFrom (hToS 12) 0 86400)
    gUTCTimeDay = fromGregorian . fromIntegral <$> gYear <*> gMonth <*> gDay
    gDiffTime = secondsToDiffTime . fromIntegral <$> gSeconds
  in
    UTCTime <$> gUTCTimeDay <*> gDiffTime


genMatch
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n RqMatch)
genMatch ps =
  if length ps < 2
  then Nothing
  else do
    genPair <- genTwoPlayerIds ps
    pure $ RqMatch
      <$> fmap fst genPair
      <*> fmap snd genPair
      <*> Gen.int (Range.linear 1 100)
      <*> Gen.int (Range.linear 1 100)
      <*> genTimeStamp

data AddMatch (v :: * -> *) =
  AddMatch RqMatch (Var Token v)
  deriving (Eq, Show)

cAddMatch
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cAddMatch env =
  let
    gen (LeaderboardState ps _as _ms) = do
      gMatch <- genMatch ps
      gToken <- genPlayerToken ps
      pure $ AddMatch <$> gMatch <*> gToken
  in
    undefined

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
