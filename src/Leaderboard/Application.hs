module Leaderboard.Application (leaderboard) where

import Control.Monad.Log (Logger)
import Servant

import Leaderboard.API
import Leaderboard.Server

leaderboard :: Environment -> Logger () -> Application
leaderboard env logger =
  serve (Proxy :: Proxy LeaderboardAPI) $
  enter (toHandler env logger) leaderboardServer
