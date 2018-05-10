module Leaderboard.TestClient where

import           Data.ByteString        (ByteString)
import           Servant.API            ((:<|>) ((:<|>)), NoContent,
                                         getResponse)
import qualified Servant.Auth.Client    as SAC (Token (Token))
import           Servant.Client         (ClientM, client)

import           Leaderboard.API        (LeaderboardAPI, leaderboardAPI)
import           Leaderboard.API.Player (AuthHeaders)
import           Leaderboard.Types      (Login, PlayerCount, RegisterPlayer,
                                         Token (..))

register      :: SAC.Token -> RegisterPlayer -> ClientM Token
registerFirst :: RegisterPlayer -> ClientM (AuthHeaders Token)
authenticate  :: Login -> ClientM (AuthHeaders Token)
playerCount   :: ClientM PlayerCount

(register :<|> registerFirst :<|> authenticate :<|> playerCount) =
  client leaderboardAPI

toServantToken
  :: AuthHeaders Token
  -> SAC.Token
toServantToken =
  SAC.Token . getToken . getResponse
