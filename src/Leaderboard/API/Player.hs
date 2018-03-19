{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API.Player where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Monad.Log                        as Log
import           Control.Monad.Reader
import           Control.Monad.Trans.Control              (MonadBaseControl)
import           Data.Aeson                               (FromJSON, parseJSON,
                                                           withObject, (.:))
import           Data.Semigroup                           ((<>))
import           Data.Text                                (Text)
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Network.HTTP.Client.TLS
import           Servant                                  ((:>), JSON, Post,
                                                           ReqBody, ServerT)
import           Servant.Auth.Server                      (Auth, AuthResult (Authenticated))
import           URI.ByteString.QQ

import           Leaderboard.Env                          (HasDbConnPool,
                                                           withConn)
import           Leaderboard.Queries                      (selectPlayerCount)
import           Leaderboard.Schema                       (Player,
                                                           PlayerT (Player))
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

playerServer
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadIO m
     , MonadReader r m
     )
  => ServerT (PlayerAPI auths) m
playerServer
  (Authenticated Player{..})
  LeaderboardRegistration{..} =
  withConn $ \c -> do
    numPlayers <- liftIO $ selectPlayerCount c
    undefined

  -- case unAuto _playerIsAdmin of
  --   Just True -> addPlayer rp
  --   _         -> throwError . ServantError $ err403
