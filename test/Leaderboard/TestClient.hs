{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Leaderboard.TestClient where

import           Data.Proxy             (Proxy)
import           Servant.API            ((:<|>) ((:<|>)), NoContent,
                                         getResponse)
import           Servant.Auth           (JWT)
import qualified Servant.Auth.Client    as SAC (Token (Token))
import           Servant.Client         (ClientM, client)

import           Leaderboard.API.Match  (MatchAPI, matchAPI)
import           Leaderboard.API.Player (AuthHeaders, PlayerAPI, playerAPI)
import           Leaderboard.Schema     (Match, MatchId)
import           Leaderboard.Types      (Login, PlayerCount, RegisterPlayer,
                                         RqMatch, Token (..))

-- TODO ajmccluskey: probs nice to have this in the main lib rather than test code.
register       :: SAC.Token -> RegisterPlayer -> ClientM Token
registerFirst  :: RegisterPlayer -> ClientM (AuthHeaders Token)
authenticate   :: Login -> ClientM (AuthHeaders Token)
getPlayerCount :: ClientM PlayerCount
(register :<|> registerFirst :<|> authenticate :<|> getPlayerCount) =
  -- Servant.Client version doesn't do cookie jar for us, so just stick to JWT
  client (playerAPI :: Proxy (PlayerAPI '[JWT]))


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

fromLbToken'
  :: AuthHeaders Token
  -> SAC.Token
fromLbToken' =
  fromLbToken . getResponse
