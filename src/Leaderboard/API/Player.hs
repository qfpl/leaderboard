{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Leaderboard.API.Player where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Log                        as Log
import           Control.Monad.Reader
import           Data.Aeson (withObject, (.:), FromJSON, parseJSON)
import           Data.Semigroup ((<>))
import           Data.Text                                (Text)
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Network.HTTP.Client.TLS
import           Servant                                  ((:>), JSON, Post,
                                                           ReqBody, Server)
import           Servant.Auth.Server                      (Auth, AuthResult)
import           URI.ByteString.QQ

import           Leaderboard.Schema                       (Player)
import           Leaderboard.Server

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

type PlayerAPI auths =
  Auth auths Player :> "register" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] Player

playerAPI
  :: Monad m
  => AuthResult Player
  -> RegisterPlayer
  -> m Player
playerAPI = undefined
