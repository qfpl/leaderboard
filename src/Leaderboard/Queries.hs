{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}

module Leaderboard.Queries
  ( selectOrPersistJwk
  , selectPlayerCount
  , addPlayer
  ) where

import           Control.Lens                             ()
import           Control.Monad                            (void)
import           Crypto.JOSE                              (JWK)
import           Crypto.Scrypt                            (Pass (Pass),
                                                           encryptPassIO')
import           Data.Aeson                               (eitherDecode')
import           Data.Aeson.Text                          (encodeToLazyText)
import           Data.Functor                             (($>))
import           Data.Maybe                               (fromMaybe,
                                                           listToMaybe)
import qualified Data.Text.Encoding                       as TE
import           Data.Text.Lazy                           (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding                  as TLE
import qualified Database.Beam                            as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as Be
import           Database.Beam.Backend.SQL.SQL92          (Sql92SelectSyntax)
import           Database.PgErrors                        (pgExceptionToError)
import           Database.PostgreSQL.Simple               (Connection)

import           Leaderboard.Schema                       (Jwk, JwkT (..),
                                                           LeaderboardDb (..),
                                                           Player,
                                                           PlayerT (Player),
                                                           leaderboardDb)
import           Leaderboard.Types                        (LeaderboardError (JwkDecodeError, MultipleJwksInDb),
                                                           RegisterPlayer (..))


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
  pure $ traverse (eitherDecode' . TLE.encodeUtf8 . fromStrict . _jwkJwk) jwks

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
  pgExceptionToError . void $
    insertValues conn (_leaderboardJwk leaderboardDb) [dbJwk]

selectPlayerCount
  :: Connection
  -> IO Integer
selectPlayerCount =
  undefined

addPlayer
  :: Connection
  -> RegisterPlayer
  -> IO (Maybe Player)
addPlayer conn LeaderboardRegistration{..} = do
  pass <- encryptPassIO' . Pass $ TE.encodeUtf8 _lbrPassword
  let
    isAdmin = fromMaybe False _lbrIsAdmin
    newPlayer = Player (B.Auto Nothing) _lbrName _lbrEmail isAdmin
  listToMaybe <$> insertValues conn (_leaderboardPlayers leaderboardDb) [newPlayer]

-- Unsure of the types for the following, and the inferred types cause compiler errors
insertValues conn table vals =
    withDb conn .
    Be.runInsertReturningList table $
    B.insertValues vals

selectList conn query =
  withDb conn .
  B.runSelectReturningList $
  B.select query

withDb =
  B.withDatabaseDebug putStrLn
