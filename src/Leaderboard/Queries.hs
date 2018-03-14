module Leaderboard.Queries
  ( getOrCreateJwk
  , addPlayer
  ) where

import           Control.Lens
import           Crypto.JOSE                (JWK)
import           Database.PostgreSQL.Simple (Connection)

import           Leaderboard.Schema         (Player)
import           Leaderboard.Types          (RegisterPlayer)

getOrCreateJwk
  :: Connection
  -> IO JWK
getOrCreateJwk conn =
  undefined

addPlayer
  :: RegisterPlayer
  -> m Player
addPlayer =
  undefined
