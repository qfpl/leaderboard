{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}
module Leaderboard.API where

import Data.Text (Text)
import Servant

import Leaderboard.API.Player
import Leaderboard.Server

type LeaderboardAPI =
  Get '[PlainText] Text :<|>
  PlayerAPI

leaderboardServer
  :: ( HasOAuth2 env
     , HasConnection env
     )
  => LServer env LeaderboardAPI
leaderboardServer =
  pure "test" :<|>
  playerAPI
