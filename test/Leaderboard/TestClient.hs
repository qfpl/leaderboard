{-# LANGUAGE DataKinds #-}

module Leaderboard.TestClient where

import           Data.Proxy             (Proxy)
import           Servant.API            ((:<|>) ((:<|>)), getResponse)
import           Servant.Auth           (JWT)
import qualified Servant.Auth.Client    as SAC (Token (Token))
import           Servant.Client         (ClientM, client)

import           Leaderboard.API.Player (AuthHeaders, PlayerAPI, playerAPI)
import           Leaderboard.Types      (Login, PlayerCount, RegisterPlayer,
                                         Token (..))

register      :: SAC.Token -> RegisterPlayer -> ClientM Token
registerFirst :: RegisterPlayer -> ClientM (AuthHeaders Token)
authenticate  :: Login -> ClientM (AuthHeaders Token)
playerCount   :: ClientM PlayerCount
(register :<|> registerFirst :<|> authenticate :<|> playerCount) =
  client (playerAPI :: Proxy (PlayerAPI '[JWT]))

toServantToken
  :: AuthHeaders Token
  -> SAC.Token
toServantToken =
  SAC.Token . getToken . getResponse
