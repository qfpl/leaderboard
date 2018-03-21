{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API where

import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Text                   (Text)
import           Servant                     ((:<|>)((:<|>)), Get, PlainText, ServerT)
import Servant.Auth.Server (AuthResult)

import           Leaderboard.API.Player      (PlayerAPI, playerServer)
import           Leaderboard.Env             (HasDbConnPool)
import           Leaderboard.Server

type LeaderboardAPI auths =
  Get '[PlainText] Text :<|>
  PlayerAPI auths

leaderboardServer
  :: ( HasDbConnPool env
     , MonadBaseControl IO m
     , MonadReader env m
     , MonadIO m
     )
  => ServerT (LeaderboardAPI auths) m
leaderboardServer =
  pure "test" :<|>
  playerServer
