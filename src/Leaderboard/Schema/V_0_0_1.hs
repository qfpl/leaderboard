{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Leaderboard.Schema.V_0_0_1 where

import           Control.Lens                   (makeLenses)
import           Data.Aeson
import           Data.ByteString                (ByteString)
import           Data.Text                      (Text)
import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Migrate
import           Servant.Auth.Server            (FromJWT, ToJWT)

data RatingT f
  = Rating
  { _ratingId         :: Columnar f (Auto Int)
  , _ratingRating     :: Columnar f Double
  , _ratingDev        :: Columnar f Double
  , _ratingVol        :: Columnar f Double
  , _ratingInactivity :: Columnar f Int
  , _ratingAge        :: Columnar f Int
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
  data PrimaryKey RatingT f = RatingId (Columnar f (Auto Int)) deriving Generic
  primaryKey = RatingId . _ratingId

instance Beamable (PrimaryKey RatingT)

data PlayerT f
  = Player
  { _playerId       :: C f (Auto Int)
  , _playerUsername :: C f (Maybe Text)
  , _playerEmail    :: C f Text
  , _playerIsAdmin  :: C f (Auto Bool)
  }
  deriving Generic

type Player = PlayerT Identity
type PlayerId = PrimaryKey PlayerT Identity

instance ToJSON Player where
  toJSON (Player (Auto a) b c d) =
    object
      [ "id" .= a
      , "username" .= b
      , "email" .= c
      , "isAdmin" .= d
      ]

instance FromJSON Player where
  parseJSON =
    withObject "Player" $ \v ->
      Player <$>
      v .: "id" <*>
      v .: "username" <*>
      v .: "email" <*>
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
  data PrimaryKey PlayerT f = PlayerId (Columnar f (Auto Int)) deriving Generic
  primaryKey = PlayerId . _playerId

instance Beamable (PrimaryKey PlayerT)


data LadderT f
  = Ladder
  { _ladderId    :: Columnar f (Auto Int)
  , _ladderName  :: Columnar f Text
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
  data PrimaryKey LadderT f = LadderId (Columnar f (Auto Int)) deriving Generic
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

data LeaderboardDb f
  = LeaderboardDb
  { _leaderboardRatings        :: f (TableEntity RatingT)
  , _leaderboardPlayers        :: f (TableEntity PlayerT)
  , _leaderboardLadders        :: f (TableEntity LadderT)
  , _leaderboardPlayerToLadder :: f (TableEntity PlayerToLadderT)
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
      (field "is_admin" boolean (defaultTo_ (val_ (Auto (Just False)))) notNull)
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
      (RatingId $ field "rating" int notNull))

makeLenses ''LeaderboardDb
makeLenses ''PlayerT
makeLenses ''RatingT
makeLenses ''LadderT
makeLenses ''PlayerToLadderT
