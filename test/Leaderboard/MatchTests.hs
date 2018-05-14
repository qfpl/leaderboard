{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Leaderboard.MatchTests
  ( matchTests
  ) where

import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Time                     (UTCTime (UTCTime),
                                                fromGregorian,
                                                secondsToDiffTime)
import           Data.Traversable              (sequenceA)
import           Database.Beam                 (Auto (..))
import           Servant.Auth.Client           (Token)
import           Servant.Client                (ClientEnv, ClientM,
                                                ServantError (..), runClientM)

import           Hedgehog                      (Callback (..),
                                                Command (Command),
                                                Concrete (Concrete), Gen,
                                                HTraversable (htraverse),
                                                MonadGen, MonadTest, PropertyT,
                                                Var (Var), annotateShow, assert,
                                                concrete, executeSequential,
                                                failure, forAll, property,
                                                (===))
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.Hedgehog           (testProperty)

import           Leaderboard.RegistrationTests (cRegister, cRegisterFirst)
import           Leaderboard.Schema            (Match (..), MatchT (..),
                                                PlayerId (..), PlayerT (..))
import qualified Leaderboard.Schema            as LS
import           Leaderboard.SharedState       (LeaderboardState (..),
                                                PlayerMap, failureClient,
                                                genPlayerRsp, successClient)
import           Leaderboard.TestClient        (MatchClient (..), fromLbToken,
                                                getPlayerCount, mkMatchClient,
                                                register, registerFirst)
import           Leaderboard.Types             (RqMatch (RqMatch),
                                                RspPlayer (..))

matchTests
  :: IO ()
  -> ClientEnv
  -> TestTree
matchTests resetDb env =
  testGroup "match" [
    propMatchTests env resetDb
  ]

genTwoPlayerIds
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n (PlayerId, PlayerId))
genTwoPlayerIds ps =
  if length ps < 2
  then Nothing
  else Just $ do
    -- Beware the Gen.just here. As long as we've checked we have enough players to satisfy generating
    -- the ids we need, then this should be fine. It's not quite partial, but the generator will fail
    -- if it retries too many times.
    let genPlayerIdJust = Gen.just . sequenceA . fmap (LS.PlayerId . _rspId) . genPlayerRsp $ ps
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
instance HTraversable AddMatch where
  htraverse f (AddMatch rm (Var ht)) = AddMatch rm . Var <$> f ht

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
    exe (AddMatch rm t) =
      successClient show env $ add (mkMatchClient (concrete t)) rm
  in
    Command gen exe [
      -- Need a token, and need a player and their opponent
      Require $ \(LeaderboardState ps _as _ms) _input -> length ps >= 2
    , Update $ \(LeaderboardState ps as ms) (AddMatch rm _t) vId ->
        LeaderboardState ps as $ M.insert vId rm ms
    , Ensure $ \(LeaderboardState _ps _as msOld) (LeaderboardState _ps' _as' msNew) _in mId -> do
        let vmId = Var (Concrete mId)
        assert $ M.member vmId msNew
        assert $ M.notMember vmId msOld
        length msNew === length msOld + 1
    ]

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
  let cs = ($ env) <$> [cRegisterFirst, cRegister, cAddMatch] --, cListMatches]
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState cs
  executeSequential initialState commands
