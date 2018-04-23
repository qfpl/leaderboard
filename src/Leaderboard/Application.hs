{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Leaderboard.Application (leaderboard) where

import           Control.Monad.Log   (Logger)
import           Data.Proxy          (Proxy (Proxy))
import           Servant             ((:~>), Application,
                                      Context ((:.), EmptyContext), Handler,
                                      enter, serveWithContext)
import           Servant.Auth        (JWT, Cookie)
import           Servant.Auth.Server (JWT, defaultCookieSettings,
                                      defaultJWTSettings)

import           Leaderboard.API     (LeaderboardAPI, leaderboardServer)
import           Leaderboard.Env     (Env, _envJWK)
import           Leaderboard.Server  (LHandlerT, toHandler)

leaderboard :: Env -> Logger () -> Application
leaderboard env logger =
  let
    jwtCfg = defaultJWTSettings (_envJWK env)
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    api = Proxy :: Proxy (LeaderboardAPI '[JWT, Cookie])
    toHandler' :: LHandlerT Env Handler :~> Handler
    toHandler' = toHandler env logger
    server = leaderboardServer defaultCookieSettings jwtCfg
  in
    serveWithContext api cfg $ enter toHandler' server
