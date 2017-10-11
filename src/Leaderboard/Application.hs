{-# language DataKinds #-}
{-# language OverloadedStrings #-}
module Leaderboard.Application (leaderboard) where

import Servant
import Data.Text (Text)

type LeaderboardAPI = Get '[PlainText] Text

leaderboardServer :: Server LeaderboardAPI
leaderboardServer = pure "test"

leaderboard :: Application
leaderboard = serve (Proxy :: Proxy LeaderboardAPI) leaderboardServer
