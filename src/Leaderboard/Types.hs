{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Leaderboard.Types where

import           Crypto.JOSE                (JWK)
import           Data.Aeson                 (FromJSON, parseJSON, withObject,
                                             (.:))
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (SqlError)
import           GHC.Generics               (Generic)
import           Servant                    (ServantErr)

data RegisterPlayer
  = LeaderboardRegistration
    { _lbrEmail    :: Text
    , _lbrName     :: Text
    , _lbrPassword :: Text
    , _lbrIsAdmin  :: Text
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
  | PostgresError SqlError
  | MultipleJwksInDb [JWK]
  | JwkDecodeError
  deriving (Eq, Show)
