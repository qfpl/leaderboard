{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Leaderboard.API where

import           Data.Text              (Text)
import           Servant

import           Leaderboard.API.Player (PlayerAPI, playerAPI)
import           Leaderboard.Server

type LeaderboardAPI auths =
  Get '[PlainText] Text :<|>
  PlayerAPI auths

leaderboardServer
  :: HasConnection env
  => LServer env (LeaderboardAPI auths)
leaderboardServer =
  pure "test" :<|>
  playerAPI
