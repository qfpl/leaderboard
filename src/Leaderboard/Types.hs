{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Leaderboard.Types where

import           Crypto.JOSE                (JWK)
import           Data.Aeson                 (FromJSON, ToJSON, parseJSON,
                                             withObject, (.:))
import           Data.Text                  (Text)
import qualified Database.PostgreSQL.Simple as Pg
import           GHC.Generics               (Generic)
import           Servant                    (ServantErr)
import           Servant.Auth.Server        (FromJWT, ToJWT)

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

newtype PlayerSession
  = PlayerSession { _psId :: Int }
    deriving (Eq, Generic, Show)
instance ToJWT PlayerSession
instance FromJWT PlayerSession
instance ToJSON PlayerSession
instance FromJSON PlayerSession
