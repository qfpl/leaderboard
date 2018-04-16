module Leaderboard.Queries
  ( selectOrPersistJwk
  , selectPlayerCount
  , addPlayer
  ) where

import           Control.Lens
import           Crypto.JOSE                (JWK)
import qualified Database.Beam              as B
import           Database.PostgreSQL.Simple (Connection)

import           Leaderboard.Schema         (Player, _leaderboardJwk, leaderboardDb)
import           Leaderboard.Types          (LeaderboardError (MultipleJwksInDb),
                                             RegisterPlayer)

withDb =
  B.withDatabaseDebug putStrLn

selectOrPersistJwk
  :: Connection
  -> IO JWK
  -> IO (Either LeaderboardError JWK)
selectOrPersistJwk conn newJwk = do
  jwks <- selectJwks conn
  case jwks of
    []    -> insertJwk conn newJwk
    [jwk] -> pure $ Right jwk
    _     -> pure $ Left MultipleJwksInDb

selectJwks
  :: Connection
  -> IO [JWK]
selectJwks conn =
  withDb conn .
  B.runSelectReturningList .
  B.select $
    B.all_ (_leaderboardJwk (leaderboardDb))

insertJwk
  :: Connection
  -> IO JWK
  -> IO (Either LeaderboardError JWK)
insertJwk =
  undefined

selectPlayerCount
  :: Connection
  -> IO Integer
selectPlayerCount =
  undefined

addPlayer
  :: RegisterPlayer
  -> m Player
addPlayer =
  undefined
