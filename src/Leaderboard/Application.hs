{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Leaderboard.Application (leaderboard) where

import           Control.Monad.Log   (Logger)
import           Data.Proxy          (Proxy (Proxy))
import           Servant             ((:~>), Application,
                                      Context ((:.), EmptyContext), Handler,
                                      enter, serveWithContext)
import           Servant.Auth        (JWT)
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
    api = Proxy :: Proxy (LeaderboardAPI '[JWT])
    toHandler' = toHandler env logger :: LHandlerT Env Handler :~> Handler
  in
    serveWithContext api cfg $ enter toHandler' leaderboardServer
