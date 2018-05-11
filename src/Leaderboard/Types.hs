{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Leaderboard.Types where

import           Control.Lens               (Lens', lens, makeLenses)
import           Control.Monad              ((<=<))
import           Crypto.JOSE                (JWK)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.=))
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Database.PgErrors          (PostgresException, tryJustPg)
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

data LeaderboardError =
    ServantError ServantErr
  | PostgresError PostgresException
  | MultipleJwksInDb [JWK]
  | JwkDecodeError
  | DbError Text
  | NoResult
  deriving (Eq, Show)

-- | Run an IO action, catch postgres exceptions, and wrap them up as a LeaderboardError.
tryJustPgError
  :: IO a
  -> IO (Either LeaderboardError a)
tryJustPgError =
  pure . first PostgresError <=< tryJustPg

data Login
  = Login
    { _loginEmail    :: Text
    , _loginPassword :: Text
    }
    deriving Generic
instance FromJSON Login
instance ToJSON Login

-- | Newtype around the Int for a player's PlayerId. When we move to a newer
-- version of beam we can hopefully use a PlayerId directly as it won't be
-- wrapping a Maybe as it does now (via Auto).
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
