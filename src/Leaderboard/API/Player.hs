{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeOperators #-}
{-# language QuasiQuotes #-}
module Leaderboard.API.Player where

import Control.Lens
import Control.Monad.Log as Log
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid
import Data.Text
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Network.HTTP.Client.TLS
import Network.OAuth.OAuth2
import Servant
import URI.ByteString.QQ

import Leaderboard.Schema
import Leaderboard.Server

data RegisterPlayer
  = GoogleRegistration
  { token :: Text
  }
  deriving Generic

instance FromJSON RegisterPlayer where

type PlayerAPI =
  "register" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] Player

data AuthResult
  = AuthResult
  { sub :: Text
  , aud :: Text
  , email :: Text
  } deriving (Generic, Show, Eq)
instance FromJSON AuthResult

getUserDetails
  :: ( MonadIO m
     , MonadReader env m
     , HasOAuth2 env
     , MonadLog () m
     , MonadError ServantErr m
     )
  => ExchangeToken
  -> m AuthResult
getUserDetails token = do
  key <- view oauth2
  manager <- liftIO newTlsManager
  maybeAccessToken <-
    liftIO $
    fetchAccessToken
      manager
      key
      token 
  case maybeAccessToken of
    Left e -> do
      Log.error $ "getUserDetails: " <> pack (show e)
      throwError err400
    Right tok -> do
      authResult <-
        liftIO $
        authGetBS'
          manager
          (accessToken tok)
          [uri|https://www.googleapis.com/oauth2/v3/tokeninfo|]
      case authResult of
        Left l -> do
          Log.error $
            "getUserDetails: " <> pack (show (l :: OAuth2Error String))
          throwError err400
        Right r ->
          case eitherDecode r of
            Left e -> do
              Log.error $ "getUserDetails: " <> pack e
              throwError err400
            Right r' ->
              if aud r' == oauthClientId key
              then pure r'
              else do
                Log.error $
                  "getUserDetails: unexpected audience " <>
                  pack (show $ aud r')
                throwError err400

playerAPI :: (HasOAuth2 env, HasConnection env) => LServer env PlayerAPI
playerAPI =
  register
  where
    register
      :: ( HasOAuth2 env
         , HasConnection env
         )
      => RegisterPlayer
      -> LHandler env Player
    register GoogleRegistration{..} = do
      res <- getUserDetails (ExchangeToken token)
      [p] <- liftQuery
        (runInsertReturningList $ leaderboardDb ^. leaderboardPlayers)
        (insertValues [Player (Auto Nothing) Nothing (email res) (sub res)])
      pure p
