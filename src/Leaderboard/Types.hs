{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Leaderboard.Types where

import           Control.Lens               (Lens', lens, makeLenses)
import           Crypto.JOSE                (JWK)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.=))
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
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
  = PlayerCount { getPlayerCount :: Integer }
  deriving (Eq, Generic, Show)
instance FromJSON PlayerCount where
  parseJSON =
    withObject "PlayerCount" $ \v ->
      PlayerCount <$> v .: "player-count"
instance ToJSON PlayerCount where
  toJSON (PlayerCount c) =
    object ["player-count" .= c]

-- | We're recreating what is already in @servant-auth-client@, however we need instances that it
-- doesn't have, so this avoids the orphans.
newtype Token
  = Token { getToken :: ByteString }
  deriving (Eq, Generic, Show)
instance FromJSON Token where
  parseJSON =
    withObject "Token" $ \v ->
      Token . encodeUtf8 <$> v .: "token"
instance ToJSON Token where
  toJSON (Token t)=
    object ["token" .= decodeUtf8 t]
