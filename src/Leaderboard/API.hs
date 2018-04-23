{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API where

import           Control.Monad.Except        (MonadError)
import           Control.Monad.Log           (MonadLog)
import           Control.Monad.Log.Label     (Label)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Servant                     (ServantErr, ServerT)
import           Servant.Auth.Server         (CookieSettings, JWTSettings)

import           Leaderboard.API.Player      (PlayerAPI, playerServer)
import           Leaderboard.Env             (HasDbConnPool)

type LeaderboardAPI auths = PlayerAPI auths

leaderboardServer
  :: ( HasDbConnPool env
     , MonadBaseControl IO m
     , MonadReader env m
     , MonadError ServantErr  m
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> ServerT (LeaderboardAPI auths) m
leaderboardServer = playerServer
