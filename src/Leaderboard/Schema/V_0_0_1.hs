{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Leaderboard.Schema.V_0_0_1 where

import           Control.Lens                             (makeLenses)
import           Crypto.JOSE                              (JWK)
import           Data.Aeson
import           Data.ByteString                          (ByteString)
import           Data.Text                                (Text)
import           Database.Beam                            (Beamable, C,
                                                           Database, Generic,
                                                           Identity, PrimaryKey,
                                                           Table (PrimaryKey, primaryKey),
                                                           TableEntity, val_)
import           Database.Beam.Backend.SQL                (SqlSerial)
import           Database.Beam.Migrate                    (CheckedDatabaseSettings,
                                                           Migration, boolean,
                                                           createTable,
                                                           defaultTo_, double,
                                                           field, int, notNull,
                                                           varchar)
import           Database.Beam.Migrate.SQL.BeamExtensions (genericSerial)
import           Database.Beam.Postgres                   (PgCommandSyntax,
                                                           PgExtensionEntity,
                                                           Postgres, pgCreateExtension, serial)
import           Database.Beam.Postgres.Migrate           ()
import           Database.Beam.Postgres.PgCrypto          (PgCrypto)
import           Servant.Auth.Server                      (FromJWT, ToJWT)

data RatingT f
  = Rating
  { _ratingId         :: C f (SqlSerial Int)
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
  data PrimaryKey RatingT f = RatingId (C f (SqlSerial Int)) deriving Generic
  primaryKey = RatingId . _ratingId

instance Beamable (PrimaryKey RatingT)

data PlayerT f
  = Player
  { _playerId       :: C f (SqlSerial Int)
  , _playerUsername :: C f Text
  , _playerEmail    :: C f Text
  , _playerIsAdmin  :: C f Bool
  }
  deriving Generic

type Player = PlayerT Identity
type PlayerId = PrimaryKey PlayerT Identity

instance ToJSON Player where
  toJSON (Player a b c d) =
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
  data PrimaryKey PlayerT f = PlayerId (C f (SqlSerial Int)) deriving Generic
  primaryKey = PlayerId . _playerId

instance Beamable (PrimaryKey PlayerT)


data LadderT f
  = Ladder
  { _ladderId    :: C f (SqlSerial Int)
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
  data PrimaryKey LadderT f = LadderId (C f (SqlSerial Int)) deriving Generic
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
  { _jwkId  :: C f (SqlSerial Int)
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
  data PrimaryKey JwkT f = JwkId (C f (SqlSerial Int)) deriving Generic
  primaryKey = JwkId . _jwkId

instance Beamable (PrimaryKey JwkT)


data LeaderboardDb f
  = LeaderboardDb
  { _leaderboardRatings        :: f (TableEntity RatingT)
  , _leaderboardPlayers        :: f (TableEntity PlayerT)
  , _leaderboardLadders        :: f (TableEntity LadderT)
  , _leaderboardPlayerToLadder :: f (TableEntity PlayerToLadderT)
  , _leaderboardJwk            :: f (TableEntity JwkT)
  , _cryptoExtension           :: f (PgExtensionEntity PgCrypto)
  }
  deriving Generic

instance Database Postgres LeaderboardDb

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
      (field "is_admin" boolean (defaultTo_ (val_ False)) notNull)
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
    ) <*>
  pgCreateExtension

makeLenses ''LeaderboardDb
makeLenses ''PlayerT
makeLenses ''RatingT
makeLenses ''LadderT
makeLenses ''PlayerToLadderT
