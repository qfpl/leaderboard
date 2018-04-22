{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API where

import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Text                   (Text)
import           Servant                     ((:<|>) ((:<|>)), Get, PlainText,
                                              ServerT, ServantErr)
import           Servant.Auth.Server         (AuthResult, CookieSettings, JWTSettings)

import           Leaderboard.API.Player      (PlayerAPI, playerServer)
import           Leaderboard.Env             (HasDbConnPool)

type LeaderboardAPI auths = PlayerAPI auths

leaderboardServer
  :: ( HasDbConnPool env
     , MonadBaseControl IO m
     , MonadReader env m
     , MonadIO m
     , MonadError ServantErr  m
     )
  => CookieSettings
  -> JWTSettings
  -> ServerT (LeaderboardAPI auths) m
leaderboardServer = playerServer
