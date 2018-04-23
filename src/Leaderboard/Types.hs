{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Leaderboard.Types where

import           Crypto.JOSE                (JWK)
import           Data.Aeson                 (FromJSON, parseJSON, withObject,
                                             (.:))
import           Data.Text                  (Text)
import qualified Database.PostgreSQL.Simple as Pg
import           GHC.Generics               (Generic)
import           Servant                    (ServantErr)

data RegisterPlayer
  = LeaderboardRegistration
    { _lbrEmail    :: Text
    , _lbrName     :: Text
    , _lbrPassword :: Text
    , _lbrIsAdmin  :: Maybe Bool
    }
  deriving Generic

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
  deriving (Eq, Show)

data PostgresException =
    PgSqlError Pg.SqlError
  | PgFormatError Pg.FormatError
  deriving (Eq, Show)
