{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Leaderboard.Application (leaderboard) where

import           Control.Monad.Log       (Logger)
import           Control.Monad.Log.Label (Label)
import           Servant                 ((:~>), Application,
                                          Context ((:.), EmptyContext), Handler,
                                          enter, serveWithContext)
import           Servant.Auth.Server     (defaultCookieSettings,
                                          defaultJWTSettings)

import           Leaderboard.API         (leaderboardServer, leaderboardAPI)
import           Leaderboard.Env         (Env, _envJWK)
import           Leaderboard.Server      (LHandlerT, toHandler)

leaderboard :: Env -> Logger Label -> Application
leaderboard env logger =
  let
    jwtCfg = defaultJWTSettings (_envJWK env)
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    toHandler' :: LHandlerT Env Handler :~> Handler
    toHandler' = toHandler env logger
    server = leaderboardServer jwtCfg
  in
    serveWithContext leaderboardAPI cfg $ enter toHandler' server
