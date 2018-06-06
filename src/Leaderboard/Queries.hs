{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
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
  , selectMatch
  , selectMatches
  ) where

import           Control.Lens                             ((^?), _head)
import           Control.Monad                            (void)
import           Control.Monad.Except                     (throwError, ExceptT (ExceptT),
                                                           runExceptT)
import           Control.Monad.IO.Class                   (liftIO)
import           Crypto.JOSE                              (JWK)
import           Crypto.Scrypt                            (EncryptedPass (..),
                                                           Pass (..),
                                                           encryptPassIO')
import           Data.Aeson                               (eitherDecode')
import           Data.Aeson.Text                          (encodeToLazyText)
import           Data.Maybe                               (fromMaybe)
import           Data.Semigroup                           ((<>))
import           Data.Text                                (Text, pack)
import qualified Data.Text.Encoding                       as TE
import           Data.Text.Lazy                           (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding                  as TLE
import qualified Database.Beam                            as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as Be
import           Database.PostgreSQL.Simple               (Connection)
import           Database.PostgreSQL.Simple.Transaction   (withTransactionLevel, IsolationLevel (Serializable))

import           Leaderboard.Lens                         (_Auto)
import           Leaderboard.Schema                       (JwkT (..),
                                                           LeaderboardDb (..),
                                                           Match, MatchT (..),
                                                           Player, PlayerT (..),
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
  -> Maybe (Connection -> IO (Either LeaderboardError ()))
  -> RegisterPlayer
  -> IO (Either LeaderboardError Player)
insertPlayer conn mp rp@LeaderboardRegistration{..} = do
  (EncryptedPass ePass) <- encryptPassIO' . Pass $ TE.encodeUtf8 _lbrPassword
  let
    isAdmin = fromMaybe False _lbrIsAdmin
    newPlayer = Player (B.Auto Nothing) _lbrUsername _lbrEmail ePass isAdmin
    noPlayerError = Left . DbError $ "Error inserting player: " <> pack (show rp)
    insert conn' =
      maybe (pure  ()) (ExceptT . ($ conn')) mp >>= \_ ->
        ExceptT $ selectPlayerByEmail conn' _lbrEmail >>= \case
          Left NoResult ->
            pure <$> insertValues conn' (_leaderboardPlayers leaderboardDb) [newPlayer]
          Left e -> pure (Left e)
          Right _ -> pure (Left PlayerExists)
  ep <- runExceptT $ serializableRetry 3 conn insert
  pure $ maybe noPlayerError Right . (^? _head) =<< ep

-- TODO ajmccluskey: return MatchId when it's not Auto (i.e. Maybe)
insertMatch
  :: Connection
  -> RqMatch
  -> IO (Either LeaderboardError Int)
insertMatch conn RqMatch{..} = do
  let
    _matchId = B.Auto Nothing
    noIdError = Left . DbError $ "No match ID returned on insert"
  ms <- tryJustPgError $ insertValues conn (_leaderboardMatches leaderboardDb) [Match{..}]
  pure $ maybe noIdError Right . (^? _head . matchId . _Auto) =<< ms

selectMatches
  :: Connection
  -> IO (Either LeaderboardError [Match])
selectMatches conn =
  tryJustPgError . selectList conn $ B.all_ (_leaderboardMatches leaderboardDb)

selectMatch
  :: Connection
  -> Int
  -> IO (Either LeaderboardError Match)
selectMatch conn mId =
  selectOne conn $
    B.filter_ (\m -> _matchId m B.==. (B.val_ . B.Auto . Just $ mId)) .
    B.all_ $ _leaderboardMatches leaderboardDb

-- | When transactions conflict, we don't get good error reporting from beam -- just an empty list
-- of results. As a workaround, this function assumes that a query failed because of transaction
-- conflicts and retries it.
serializableRetry
  :: Int
  -> Connection
  -> (Connection -> ExceptT LeaderboardError IO [a])
  -> ExceptT LeaderboardError IO [a]
serializableRetry attempts conn act =
  let
    run =
      ExceptT . withTransactionLevel Serializable conn . runExceptT $ act conn
    loop n =
      if n == 0
      then throwError NoResult
      else run >>= \case
        [] -> loop (n - 1)
        vs -> pure vs
  in
    loop attempts

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
