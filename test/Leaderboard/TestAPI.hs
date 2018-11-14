{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.TestAPI where

import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Log           (MonadLog)
import qualified Control.Monad.Log           as Log
import           Control.Monad.Log.Label     (Label (Label), withLabel)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (FromJSON (parseJSON),
                                              ToJSON (toJSON), object,
                                              withObject, (.:), (.=))
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)
import           Servant                     ((:<|>) ((:<|>)), (:>), Get, JSON,
                                              ServantErr, ServerT, err500,
                                              errBody)
import           Servant.Auth.Server         (JWTSettings)

--import Leaderboard.API.Player (Pl)
import           Leaderboard.API             (LeaderboardAPI, leaderboardServer)
import           Leaderboard.Env             (HasDbConnPool, withConn)
import           Leaderboard.Queries         (selectPlayerCount)

type TestEndpoints =
  "player" :> "count" :> Get '[JSON] PlayerCount

type TestAPI auths =
       LeaderboardAPI auths
  :<|> TestEndpoints

testEndpoints ::
  Proxy TestEndpoints
testEndpoints = Proxy

testAPI ::
  Proxy (TestAPI auths)
testAPI = Proxy

testServer ::
  ( HasDbConnPool env
  , MonadBaseControl IO m
  , MonadReader env m
  , MonadError ServantErr  m
  , MonadLog Label m
  )
  => JWTSettings
  -> ServerT (TestAPI auths) m
testServer jwtSettings =
       (leaderboardServer jwtSettings)
  :<|> count

count
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadError ServantErr m
     , MonadReader r m
     , MonadLog Label m
     )
  => m PlayerCount
count =
  withLabel (Label "/count") $ PlayerCount <$> getPlayerCount

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

getPlayerCount
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadError ServantErr m
     , MonadReader r m
     , MonadLog Label m
     )
  => m Integer
getPlayerCount = do
  let
    throwNoPlayerCount e = do
      Log.error $ "Error retrieving player count: " <> (T.pack . show $ e)
      throwError err500 { errBody = "Error retrieving player count" }
  en <- withConn $ liftIO . selectPlayerCount
  either throwNoPlayerCount pure en
