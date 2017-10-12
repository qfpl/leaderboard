import Network.Wai.Handler.Warp

import Database.PostgreSQL.Simple
import Leaderboard.Application (leaderboard)

main = do
  conn <- connect _
  run 8080 $ leaderboard conn
