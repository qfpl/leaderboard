{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Leaderboard.API.Player where

import           Control.Lens
-- import           Control.Monad.Except
import Control.Monad.Error (MonadError, throwError)
import           Control.Monad.Log                        as Log
import           Control.Monad.Reader
import           Data.Bool                                (bool)
import           Data.Semigroup                           ((<>))
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Network.HTTP.Client.TLS
import           Servant                                  ((:>), JSON, Post,
                                                           ReqBody, Server,
                                                           err403)
import           Servant.Auth.Server                      (Auth, AuthResult (Authenticated))
import           URI.ByteString.QQ

import Leaderboard.Queries (addPlayer)
import           Leaderboard.Schema                       (Player,
                                                           PlayerT (Player), _playerIsAdmin)
import           Leaderboard.Server
import Leaderboard.Types (LeaderboardError (ServantError), RegisterPlayer)

type PlayerAPI auths =
  Auth auths Player :> "register" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] Player

playerAPI
  :: ( Monad m
     , MonadError LeaderboardError m
     )
  => AuthResult Player
  -> RegisterPlayer
  -> m Player
playerAPI (Authenticated (Player{..})) rp =
  case unAuto _playerIsAdmin of
    Just True -> addPlayer rp
    _         -> throwError . ServantError $ err403
