{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Leaderboard.Types where

import           Control.Lens               (makeLenses, Lens', lens)
import           Crypto.JOSE                (JWK)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.=))
import           Data.Text                  (Text)
import qualified Database.PostgreSQL.Simple as Pg
import           GHC.Generics               (Generic)
import           Servant                    (ServantErr)
import           Servant.Auth.Server        (FromJWT, ToJWT)

data Command
  = RunApp
  | MigrateDb
  deriving (Eq, Show)

data ApplicationOptions
  = ApplicationOptions
  { _dbConnInfo :: Pg.ConnectInfo
  , _port       :: Int
  , _command    :: Command
  }
  deriving (Eq, Show)
makeLenses ''ApplicationOptions

_connectDatabase
 :: Lens' Pg.ConnectInfo String
_connectDatabase =
  lens
    Pg.connectDatabase
    (\ci name -> ci { Pg.connectDatabase = name })

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

data LeaderboardError =
    ServantError ServantErr
  | PostgresError PostgresException
  | MultipleJwksInDb [JWK]
  | JwkDecodeError
  | DbError Text
  | NoResult
  deriving (Eq, Show)

data PostgresException =
    PgSqlError Pg.SqlError
  | PgFormatError Pg.FormatError
  deriving (Eq, Show)

data Login
  = Login
    { _loginEmail    :: Text
    , _loginPassword :: Text
    }
    deriving Generic
instance FromJSON Login
instance ToJSON Login

newtype PlayerSession
  = PlayerSession { _psId :: Int }
    deriving (Eq, Generic, Show)
instance ToJWT PlayerSession
instance FromJWT PlayerSession
instance ToJSON PlayerSession
instance FromJSON PlayerSession

newtype PlayerCount
  = PlayerCount Integer
  deriving (Eq, Generic, Show)

instance FromJSON PlayerCount
instance ToJSON PlayerCount
