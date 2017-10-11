import Network.Wai.Handler.Warp

import Leaderboard.Application (leaderboard)

main = run 8080 leaderboard
