module Leaderboard.TestClient where

import           Data.ByteString        (ByteString)
import           Servant.API            ((:<|>) ((:<|>)), NoContent)
import qualified Servant.Auth.Client    as Client (Token)
import           Servant.Client         (ClientM, client)

import           Leaderboard.API        (LeaderboardAPI, leaderboardAPI)
import           Leaderboard.API.Player (AuthHeaders)
import           Leaderboard.Types      (Login, PlayerCount, RegisterPlayer,
                                         Token)

lcRegister      :: Client.Token -> RegisterPlayer -> ClientM Token
lcRegisterFirst :: RegisterPlayer -> ClientM (AuthHeaders Token)
lcAuthenticate  :: Login -> ClientM (AuthHeaders Token)
lcPlayerCount   :: ClientM PlayerCount

(lcRegister :<|> lcRegisterFirst :<|> lcAuthenticate :<|> lcPlayerCount) =
  client leaderboardAPI
