{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Leaderboard.TestClient where

import           Data.Proxy             (Proxy)
import           Servant.API            ((:<|>) ((:<|>)), NoContent)
import           Servant.Auth           (JWT)
import qualified Servant.Auth.Client    as SAC (Token (Token))
import           Servant.Client         (ClientM, client)

import           Leaderboard.API.Match  (MatchAPI, matchAPI)
import           Leaderboard.API.Player (PlayerAPI, playerAPI)
import           Leaderboard.Schema     (Match, MatchId, Player)
import           Leaderboard.Types      (Login, RegisterPlayer, ResponsePlayer,
                                         RqMatch, Token (..))

import           Leaderboard.TestAPI    (PlayerCount, testEndpoints)

-- TODO ajmccluskey: probs nice to have this in the main lib rather than test code.
register       :: SAC.Token -> RegisterPlayer -> ClientM ResponsePlayer
registerFirst  :: RegisterPlayer -> ClientM ResponsePlayer
me             :: SAC.Token -> ClientM Player
authenticate   :: Login -> ClientM Token
getPlayerCount :: ClientM PlayerCount

(register :<|> registerFirst :<|> me :<|> authenticate) =
  -- Servant.Client version doesn't do cookie jar for us, so just stick to JWT
  client (playerAPI :: Proxy (PlayerAPI '[JWT]))

getPlayerCount = client testEndpoints

data MatchClient =
  MatchClient
  { add    ::  RqMatch -> ClientM Int
  , list   :: ClientM [Match]
  , get    :: MatchId -> ClientM Match
  , delete :: MatchId -> ClientM NoContent
  , edit   :: MatchId -> RqMatch -> ClientM NoContent
  }

mkMatchClient
  :: SAC.Token
  -> MatchClient
mkMatchClient t =
  let
    (add :<|> list :<|> get :<|> delete :<|> edit) =
      client (matchAPI :: Proxy (MatchAPI '[JWT])) t
  in
    MatchClient{..}

fromLbToken
  :: Token
  -> SAC.Token
fromLbToken =
  SAC.Token . getToken
