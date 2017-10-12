module Leaderboard.Application (leaderboard) where

import Database.Beam.Postgres
import Servant

import Leaderboard.API
import Leaderboard.Server

leaderboard :: Connection -> Application
leaderboard conn =
  serve (Proxy :: Proxy LeaderboardAPI) $
  enter (toHandler conn) leaderboardServer
