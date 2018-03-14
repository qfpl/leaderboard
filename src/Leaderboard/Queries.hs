module Leaderboard.Queries
  (getOrCreateJwk)
where

import           Control.Lens
import           Crypto.JOSE                (JWK)
import           Database.PostgreSQL.Simple

import           Leaderboard.Schema         (Player)

getOrCreateJwk
  :: Connection
  -> IO JWK
getOrCreateJwk conn =
  undefined
