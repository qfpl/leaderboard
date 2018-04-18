module Leaderboard.Queries
  ( selectOrPersistJwk
  , selectPlayerCount
  , addPlayer
  ) where

import           Control.Lens
import           Crypto.JOSE                (JWK)
import           Data.Aeson                 (eitherDecode')
import           Data.Aeson.Text            (encodeToLazyText)
import           Data.Functor               (($>))
import           Data.Text.Lazy             (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8, encodeUtf8)
import qualified Database.Beam              as B
import           Database.PgErrors          (pgExceptionToError)
import           Database.PostgreSQL.Simple (Connection)

import           Leaderboard.Schema         (Jwk, JwkT (..), Player,
                                             leaderboardDb, _leaderboardJwk)
import           Leaderboard.Types          (LeaderboardError (JwkDecodeError, MultipleJwksInDb),
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
    Left s      -> pure . Left $ JwkDecodeError
    Right []    -> newJwk >>= \jwk -> (jwk <$) <$> insertJwk conn newJwk
    Right [jwk] -> pure $ Right jwk
    Right jwks  -> pure . Left . MultipleJwksInDb $ jwks

selectJwks
  :: Connection
  -> IO (Either String [JWK])
selectJwks conn = do
  jwks <-
    withDb conn .
    B.runSelectReturningList .
    B.select $
      B.all_ (_leaderboardJwk leaderboardDb)
  pure $ traverse (eitherDecode' . encodeUtf8 . fromStrict . _jwkJwk) jwks

insertJwk
  :: Connection
  -> IO JWK
  -> IO (Either LeaderboardError ())
insertJwk conn jwk = do
  jwk' <- jwk
  let
    dbJwk =
      Jwk { _jwkId = B.Auto Nothing
          , _jwkJwk = toStrict . encodeToLazyText $ jwk'
          }
  pgExceptionToError .
    withDb conn .
    B.runInsert .
    B.insert (_leaderboardJwk leaderboardDb) $
    B.insertValues [dbJwk]

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
