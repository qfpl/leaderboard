{-# LANGUAGE ScopedTypeVariables #-}

module Leaderboard.Gens where

import           Hedgehog                (MonadGen, Var)
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range

import           Control.Lens            ((&), (^.))
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Data.Time               (LocalTime, UTCTime (UTCTime),
                                          fromGregorian, secondsToDiffTime, utc,
                                          utcToLocalTime)
import           Database.Beam           (Auto (Auto))

import           Leaderboard.Schema      (PlayerId)
import qualified Leaderboard.Schema      as LS
import           Leaderboard.SharedState (HasAdmins, PlayerMap, PlayerWithRsp,
                                          TestRsp, admins, HasPlayers, players)
import           Leaderboard.Types       (RegisterPlayer (LeaderboardRegistration, _lbrEmail),
                                          RqMatch (RqMatch))

genRegPlayerUniqueEmail ::
  ( MonadGen n
  , HasPlayers state
  )
  => state v
  -> n RegisterPlayer
genRegPlayerUniqueEmail state =
  let
    playerEmails = state ^. players & M.keysSet
  in
    Gen.filter ((`S.notMember` playerEmails) . _lbrEmail) genRegPlayer

genRegPlayer
  :: MonadGen n
  => n RegisterPlayer
genRegPlayer =
  let
    genNonEmptyAlphaNum = Gen.text (Range.linear 1 20) Gen.alphaNum
  in
    LeaderboardRegistration
      <$> genNonEmptyAlphaNum
      <*> genNonEmptyAlphaNum
      <*> genNonEmptyAlphaNum
      <*> Gen.maybe Gen.bool


-- | Generate a UTC time stamp stored as LocalTime. @beam-postgres@ barfs on UTCTime
-- so doing this as a workaround for now.
genTimestamp
  :: MonadGen n
  => n LocalTime
genTimestamp =
  let
    gYear = Gen.int (Range.linearFrom 1900 1970 2500)
    gMonth = Gen.int (Range.linear 1 12)
    -- fromGregorian automatically trims to valid dates, so 2001-02-31 becomes 2001-02-28
    gDay = Gen.int (Range.linear 1 31)
    hToS = (* 3600)
    gSeconds = Gen.int (Range.linearFrom (hToS 12) 0 86400)
    gUTCTimeDay = fromGregorian . fromIntegral <$> gYear <*> gMonth <*> gDay
    gDiffTime = secondsToDiffTime . fromIntegral <$> gSeconds
  in
    fmap (utcToLocalTime utc) . UTCTime <$> gUTCTimeDay <*> gDiffTime

genPlayerId
  :: MonadGen n
  => n PlayerId
genPlayerId =
  LS.PlayerId . Auto . pure <$> Gen.int (Range.linear 0 (maxBound :: Int))

genRqMatch
  :: MonadGen n
  => n RqMatch
genRqMatch = do
  p1 <- genPlayerId
  p2 <- Gen.filter (/= p1) genPlayerId
  score1 <- Gen.int (Range.linear 23 100)
  score2 <- (+ score1) <$> Gen.element [2, -2]
  RqMatch p1 p2 score1 score2 <$> genTimestamp

genAdminWithRsp
  :: ( MonadGen n
     , HasAdmins s
     )
  => s v
  -> Maybe (n (Var TestRsp v))
genAdminWithRsp s =
  -- TODO ajmccluskey: be better
  -- Emails in admin _must_ be a subset of those in players. Without a Traversable
  -- instance for Gen I couldn't make this be not partial.
  if null (s ^. admins)
  then Nothing
  else Just $
    s ^. admins & Gen.element . S.toList

genPlayerWithRsp
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n (PlayerWithRsp v))
genPlayerWithRsp ps =
  if null ps
  then Nothing
  else pure . fmap snd . Gen.element . M.toList $ ps
