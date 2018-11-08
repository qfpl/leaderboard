{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Leaderboard.Types where

import           Control.Lens               (Lens', lens, makeLenses, makeClassy)
import           Control.Monad              ((<=<))
import qualified Control.Monad.Log          as Log
import           Crypto.JOSE                (JWK)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.=))
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Time                  (LocalTime)
import           Database.PgErrors          (PostgresException, tryJustPg)
import qualified Database.PostgreSQL.Simple as Pg
import           GHC.Generics               (Generic)
import           Servant                    (ServantErr)
import           Servant.Auth.Server        (FromJWT, ToJWT)

import           Leaderboard.Schema         (PlayerId)

data Command
  = RunApp
  | MigrateDb
  deriving (Eq, Show)

data ApplicationOptions
  = ApplicationOptions
  { _dbConnInfo :: Pg.ConnectInfo
  , _port       :: Int
  , _command    :: Command
  , _logLevel   :: Maybe Log.Level
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
  | PlayerExists
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
  = PlayerCount { unPlayerCount :: Integer }
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
  deriving (Eq, Ord, Generic, Show)
instance FromJSON Token where
  parseJSON =
    withObject "Token" $ \v ->
      Token . encodeUtf8 <$> v .: "token"
instance ToJSON Token where
  toJSON (Token t)=
    object ["token" .= decodeUtf8 t]

-- TODO ajmccluskey: newtype these!
data RegisterPlayer
  = LeaderboardRegistration
    { _lbrEmail    :: Text
    , _lbrUsername :: Text
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
    , "name" .= _lbrUsername
    , "password" .= _lbrPassword
    , "isAdmin" .= _lbrIsAdmin
    ]


data ResponsePlayer =
  ResponsePlayer
  { _rspId    :: PlayerId
  , _rspToken :: Token
  }
  deriving (Eq, Ord, Show)

instance FromJSON ResponsePlayer where
  parseJSON =
    withObject "ResponsePlayer" $ \v ->
      ResponsePlayer <$>
      v .: "id" <*>
      v .: "token"

instance ToJSON ResponsePlayer where
  toJSON ResponsePlayer{..} =
    object
    [ "id" .= _rspId
    , "token" .= _rspToken
    ]


-- | Match used in requests.
data RqMatch
  = RqMatch
  { _matchPlayer1      :: PlayerId
  , _matchPlayer2      :: PlayerId
  , _matchPlayer1Score :: Int
  , _matchPlayer2Score :: Int
  , _matchTime         :: LocalTime
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

instance ToJSON RqMatch where
  toJSON RqMatch{..} =
    object
    [ "player1" .= _matchPlayer1
    , "player2" .= _matchPlayer2
    , "player1Score" .= _matchPlayer1Score
    , "player2Score" .= _matchPlayer2Score
    , "time" .= _matchTime
    ]

makeClassy ''ResponsePlayer
