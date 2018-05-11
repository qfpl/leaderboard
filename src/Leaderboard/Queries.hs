{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- There are helpers in here with gnarly types that would be inferred if we
-- copy pasted the code into each call site.
{-# OPTIONS_GHC -fno-warn-missing-signatures#-}

module Leaderboard.Queries
  ( selectOrPersistJwk
  -- * Player
  , selectPlayerCount
  , selectPlayerById
  , selectPlayerByEmail
  , insertPlayer
  -- * Matche
  , insertMatch
  ) where

import           Control.Lens                             ((^?), _Just)
import           Control.Monad                            (void)
import           Crypto.JOSE                              (JWK)
import           Crypto.Scrypt                            (EncryptedPass (..),
                                                           Pass (..),
                                                           encryptPassIO')
import           Data.Aeson                               (eitherDecode')
import           Data.Aeson.Text                          (encodeToLazyText)
import           Data.Maybe                               (fromMaybe,
                                                           listToMaybe)
import           Data.Text                                (Text)
import qualified Data.Text.Encoding                       as TE
import           Data.Text.Lazy                           (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding                  as TLE
import qualified Database.Beam                            as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as Be
import           Database.PostgreSQL.Simple               (Connection)

import           Leaderboard.Lens                         (_Auto)
import           Leaderboard.Schema                       (JwkT (..),
                                                           LeaderboardDb (..),
                                                           MatchT (..), Player,
                                                           PlayerT (..),
                                                           leaderboardDb,
                                                           matchId)
import           Leaderboard.Types                        (LeaderboardError (..),
                                                           RegisterPlayer (..),
                                                           RqMatch (..),
                                                           tryJustPgError)


selectOrPersistJwk
  :: Connection
  -> IO JWK
  -> IO (Either LeaderboardError JWK)
selectOrPersistJwk conn newJwk = do
  eJwks <- selectJwks conn
  case eJwks of
    Left _      -> pure . Left $ JwkDecodeError
    Right []    -> newJwk >>= \jwk -> (jwk <$) <$> insertJwk conn newJwk
    Right [jwk] -> pure $ Right jwk
    Right jwks  -> pure . Left . MultipleJwksInDb $ jwks

selectJwks
  :: Connection
  -> IO (Either String [JWK])
selectJwks conn = do
  jwks <- selectList conn $ B.all_ (_leaderboardJwk leaderboardDb)
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
  tryJustPgError . void $
    insertValues conn (_leaderboardJwk leaderboardDb) [dbJwk]

selectPlayerCount
  :: Connection
  -> IO (Either LeaderboardError Integer)
selectPlayerCount conn =
  maybe (Left . DbError $ "Player count returned `Nothing`") (Right . fromIntegral) <$>
    countAll conn (B.all_ $ _leaderboardPlayers leaderboardDb)

selectPlayerById
  :: Connection
  -> Int
  -> IO (Either LeaderboardError Player)
selectPlayerById conn pId =
  selectOne conn .
    B.filter_ (\p -> _playerId p B.==. (B.val_ . B.Auto . Just $ pId)) .
    B.all_ $ _leaderboardPlayers leaderboardDb

selectPlayerByEmail
  :: Connection
  -> Text
  -> IO (Either LeaderboardError Player)
selectPlayerByEmail conn email =
  selectOne conn .
    B.filter_ (\p -> _playerEmail p B.==. B.val_ email) .
    B.all_ $ _leaderboardPlayers leaderboardDb

insertPlayer
  :: Connection
  -> RegisterPlayer
  -> IO (Maybe Player)
insertPlayer conn LeaderboardRegistration{..} = do
  (EncryptedPass ePass) <- encryptPassIO' . Pass $ TE.encodeUtf8 _lbrPassword
  let
    isAdmin = fromMaybe False _lbrIsAdmin
    newPlayer = Player (B.Auto Nothing) _lbrName _lbrEmail ePass isAdmin
  listToMaybe <$> insertValues conn (_leaderboardPlayers leaderboardDb) [newPlayer]

-- TODO ajmccluskey: return MatchId when it's not Auto (i.e. Maybe)
insertMatch
  :: Connection
  -> RqMatch
  -> IO (Maybe Int)
insertMatch conn RqMatch{..} = do
  let
    _matchId = B.Auto Nothing
  ms <- insertValues conn (_leaderboardMatches leaderboardDb) [Match{..}]
  pure $ listToMaybe ms ^? _Just . matchId . _Auto

-- Unsure of the types for the following, and the inferred types cause compiler errors
insertValues conn table vals =
    withDb conn .
    Be.runInsertReturningList table $
    B.insertValues vals

selectList conn query =
  withDb conn .
  B.runSelectReturningList $
  B.select query

selectOne conn query = do
  ma <- withDb conn .
        B.runSelectReturningOne $
        B.select query
  case ma of
    Nothing -> pure $ Left NoResult
    Just a  -> pure $ Right a

countAll conn table =
  withDb conn .
  B.runSelectReturningOne .
  B.select $ B.aggregate_  (const B.countAll_) table

withDb =
  B.withDatabase --Debug putStrLn
