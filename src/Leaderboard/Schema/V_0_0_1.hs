{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Leaderboard.Schema.V_0_0_1 where

import           Control.Lens                   (makeLenses)
import           Data.Aeson                     (FromJSON (..), ToJSON (..),
                                                 Value (Null, Number), object,
                                                 withObject, (.:), (.=))
import           Data.ByteString                (ByteString)
import           Data.Text                      (Text)
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import           Data.Time                      (UTCTime)
import           Database.Beam                  (Auto (Auto), Beamable, C,
                                                 Database, Generic, Identity,
                                                 PrimaryKey,
                                                 Table (PrimaryKey, primaryKey),
                                                 TableEntity)
import           Database.Beam.Migrate          (CheckedDatabaseSettings,
                                                 Migration, createTable, double,
                                                 field, int, notNull, timestamp,
                                                 varchar)

import           Database.Beam.Postgres         (PgCommandSyntax, Postgres)
import           Database.Beam.Postgres.Migrate (boolean, bytea, serial)
import           Servant                        (FromHttpApiData (parseUrlPiece))
import           Servant.Auth.Server            (FromJWT, ToJWT)

data RatingT f
  = Rating
  { _ratingId         :: C f (Auto Int)
  , _ratingRating     :: C f Double
  , _ratingDev        :: C f Double
  , _ratingVol        :: C f Double
  , _ratingInactivity :: C f Int
  , _ratingAge        :: C f Int
  }
  deriving Generic

type Rating = RatingT Identity
type RatingId = PrimaryKey RatingT Identity

deriving instance Eq Rating
deriving instance Show Rating
deriving instance Ord Rating

deriving instance Eq RatingId
deriving instance Show RatingId
deriving instance Ord RatingId

instance Beamable RatingT
instance Table RatingT where
  data PrimaryKey RatingT f = RatingId (C f (Auto Int)) deriving Generic
  primaryKey = RatingId . _ratingId

instance Beamable (PrimaryKey RatingT)

data PlayerT f
  = Player
  { _playerId       :: C f (Auto Int)
  , _playerUsername :: C f Text
  , _playerEmail    :: C f Text
  , _playerPassword :: C f ByteString
  , _playerIsAdmin  :: C f Bool
  }
  deriving Generic

type Player = PlayerT Identity
type PlayerId = PrimaryKey PlayerT Identity

instance FromJSON PlayerId where
  parseJSON =
    fmap (PlayerId . Auto . pure) . parseJSON

instance ToJSON PlayerId where
  toJSON (PlayerId (Auto mId)) =
    maybe Null (Number . fromIntegral) mId

instance ToJSON Player where
  toJSON (Player a b c d e) =
    object
      [ "id" .= a
      , "username" .= b
      , "email" .= c
      , "password" .= decodeUtf8 d
      , "isAdmin" .= e
      ]

instance FromJSON Player where
  parseJSON =
    withObject "Player" $ \v ->
      Player <$>
      v .: "id" <*>
      v .: "username" <*>
      v .: "email" <*>
      (encodeUtf8 <$> v .: "password") <*>
      v .: "isAdmin"

instance ToJWT Player
instance FromJWT Player
deriving instance Eq Player
deriving instance Show Player
deriving instance Ord Player

deriving instance Eq PlayerId
deriving instance Show PlayerId
deriving instance Ord PlayerId

instance Beamable PlayerT
instance Table PlayerT where
  data PrimaryKey PlayerT f = PlayerId (C f (Auto Int)) deriving Generic
  primaryKey = PlayerId . _playerId

instance Beamable (PrimaryKey PlayerT)


data MatchT f
  = Match
  { _matchId           :: C f (Auto Int)
  , _matchPlayer1      :: PrimaryKey PlayerT f
  , _matchPlayer2      :: PrimaryKey PlayerT f
  , _matchPlayer1Score :: C f Int
  , _matchPlayer2Score :: C f Int
  , _matchTime         :: C f UTCTime
  }
  deriving (Generic)

instance Beamable MatchT
instance Table MatchT where
  data PrimaryKey MatchT f = MatchId (C f (Auto Int)) deriving Generic
  primaryKey = MatchId . _matchId
instance Beamable (PrimaryKey MatchT)

type Match = MatchT Identity
deriving instance Eq Match
deriving instance Show Match
deriving instance Ord Match

type MatchId = PrimaryKey MatchT Identity
deriving instance Eq MatchId
deriving instance Show MatchId
deriving instance Ord MatchId

instance FromHttpApiData MatchId where
  parseUrlPiece = fmap (MatchId . Auto . pure) . parseUrlPiece

instance ToJSON Match where
  toJSON Match{..} =
    object
    [ "id" .= _matchId
    , "player1" .= _matchPlayer1
    , "player2" .= _matchPlayer2
    , "player1Score" .= _matchPlayer1Score
    , "player2Score" .= _matchPlayer2Score
    , "timestamp" .= _matchTime
    ]


data LadderT f
  = Ladder
  { _ladderId    :: C f (Auto Int)
  , _ladderName  :: C f Text
  , _ladderOwner :: PrimaryKey PlayerT f
  }
  deriving Generic

deriving instance Eq Ladder
deriving instance Show Ladder
deriving instance Ord Ladder

deriving instance Eq LadderId
deriving instance Show LadderId
deriving instance Ord LadderId

type Ladder = LadderT Identity
type LadderId = PrimaryKey LadderT Identity

instance Beamable LadderT
instance Table LadderT where
  data PrimaryKey LadderT f = LadderId (C f (Auto Int)) deriving Generic
  primaryKey = LadderId . _ladderId

instance Beamable (PrimaryKey LadderT)


data PlayerToLadderT f
  = PlayerToLadder
  { _p2lPlayer :: PrimaryKey PlayerT f
  , _p2lLadder :: PrimaryKey LadderT f
  , _p2lRating :: PrimaryKey RatingT f
  }
  deriving Generic

deriving instance Eq PlayerToLadder
deriving instance Show PlayerToLadder
deriving instance Ord PlayerToLadder

type PlayerToLadder = PlayerToLadderT Identity

instance Beamable PlayerToLadderT
instance Table PlayerToLadderT where
  data PrimaryKey PlayerToLadderT f
    = PlayerToLadderKey (PrimaryKey PlayerT f) (PrimaryKey LadderT f)
      deriving Generic
  primaryKey = PlayerToLadderKey <$> _p2lPlayer <*> _p2lLadder

instance Beamable (PrimaryKey PlayerToLadderT)


data JwkT f
  = Jwk
  { _jwkId  :: C f (Auto Int)
  , _jwkJwk :: C f Text
  }
  deriving Generic

deriving instance Eq Jwk
deriving instance Show Jwk
deriving instance Ord Jwk

type Jwk = JwkT Identity
type JwkId = PrimaryKey JwkT Identity

instance Beamable JwkT
instance Table JwkT where
  data PrimaryKey JwkT f = JwkId (C f (Auto Int)) deriving Generic
  primaryKey = JwkId . _jwkId

instance Beamable (PrimaryKey JwkT)


data LeaderboardDb f
  = LeaderboardDb
  { _leaderboardRatings        :: f (TableEntity RatingT)
  , _leaderboardPlayers        :: f (TableEntity PlayerT)
  , _leaderboardMatches        :: f (TableEntity MatchT)
  , _leaderboardLadders        :: f (TableEntity LadderT)
  , _leaderboardPlayerToLadder :: f (TableEntity PlayerToLadderT)
  , _leaderboardJwk            :: f (TableEntity JwkT)
  }
  deriving Generic

instance Database LeaderboardDb

migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres LeaderboardDb)
migration () =
  LeaderboardDb <$>
  createTable "ratings"
    (Rating
      (field "id" serial)
      (field "rating" double notNull)
      (field "dev" double notNull)
      (field "vol" double notNull)
      (field "inactivity" int notNull)
      (field "age" int notNull)) <*>
  createTable "players"
    (Player
      (field "id" serial)
      (field "name" (varchar Nothing) notNull)
      (field "email" (varchar Nothing) notNull)
      (field "password" bytea notNull)
      (field "is_admin" boolean notNull)
    ) <*>
  createTable "matches"
    (Match
      (field "id" serial)
      (PlayerId $ field "player1" int notNull)
      (PlayerId $ field "player2" int notNull)
      (field "player1score" int notNull)
      (field "player2score" int notNull)
      (field "time" timestamp notNull)
    ) <*>
  createTable "ladders"
    (Ladder
      (field "id" serial)
      (field "name" (varchar Nothing) notNull)
      (PlayerId $ field "owner" int notNull)
    ) <*>
  createTable "playerToLadder"
    (PlayerToLadder
      (PlayerId $ field "player" int notNull)
      (LadderId $ field "ladder" int notNull)
      (RatingId $ field "rating" int notNull)
    ) <*>
  createTable "jwk"
    (Jwk
      (field "id" serial)
      (field "jwk" (varchar Nothing) notNull)
    )

-------------------------------------------------------------------------------
-- These are user facing types that relate to types from the schema, so we keep
-- them together
-------------------------------------------------------------------------------

-- TODO ajmccluskey: newtype these!
data RegisterPlayer
  = LeaderboardRegistration
    { _lbrEmail    :: Text
    , _lbrName     :: Text
    , _lbrPassword :: Text
    , _lbrIsAdmin  :: Maybe Bool
    }
  deriving (Eq, Generic, Show)

instance FromJSON RegisterPlayer where
  parseJSON =
    withObject "RegisterPlayer" $ \v ->
      LeaderboardRegistration <$>
      v .: "email" <*>
      v .: "name" <*>
      v .: "password" <*>
      v .: "isAdmin"

instance ToJSON RegisterPlayer where
  toJSON LeaderboardRegistration{..} =
    object
    [ "email" .= _lbrEmail
    , "name" .= _lbrName
    , "password" .= _lbrPassword
    , "isAdmin" .= _lbrIsAdmin
    ]

-- | Match used in requests.
data RqMatch
  = RqMatch
  { _matchPlayer1      :: PlayerId
  , _matchPlayer2      :: PlayerId
  , _matchPlayer1Score :: Int
  , _matchPlayer2Score :: Int
  , _matchTime         :: UTCTime
  }
  deriving (Eq, Show)

instance FromJSON RqMatch where
  parseJSON =
    withObject "RqMatch" $ \v ->
      RqMatch
      <$> v .: "player1"
      <*> v .: "player2"
      <*> v .:  "player1Score"
      <*> v .:  "player2Score"
      <*> v .:  "time"

makeLenses ''LeaderboardDb
makeLenses ''PlayerT
makeLenses ''RatingT
makeLenses ''LadderT
makeLenses ''PlayerToLadderT
