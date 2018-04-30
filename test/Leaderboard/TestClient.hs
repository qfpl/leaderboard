{-# LANGUAGE RecordWildCards #-}

module Leaderboard.TestClient where

import           Servant.API            ((:<|>) ((:<|>)), NoContent)
import           Servant.Auth.Client    (Token)
import           Servant.Client         (ClientM, client)

import           Leaderboard.API        (LeaderboardAPI, leaderboardAPI)
import           Leaderboard.API.Player (AuthHeaders)
import           Leaderboard.Types      (Login, RegisterPlayer)

data LeaderboardClient =
  LeaderboardClient
  { lcRegisterFirst :: RegisterPlayer -> ClientM (AuthHeaders NoContent)
  , lcRegister      :: Token -> RegisterPlayer -> ClientM NoContent
  , lcLogin         :: Login -> ClientM (AuthHeaders NoContent)
  }

mkLeaderboardClient
  :: LeaderboardClient
mkLeaderboardClient =
  let
    (lcRegister :<|> lcRegisterFirst :<|> lcLogin) =
      client leaderboardAPI
  in
    LeaderboardClient{..}
