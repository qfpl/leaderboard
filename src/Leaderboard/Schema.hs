{-# LANGUAGE OverloadedStrings #-}

module Leaderboard.Schema
  ( module Leaderboard.Schema.V_0_0_1
  , migration
  , leaderboardDb
  , createSchema
  )
where

import           Control.Exception           (catch)
import           Control.Monad               (void)
import           Database.Beam
import           Database.Beam.Backend       (runNoReturn)
import           Database.Beam.Migrate       (CheckedDatabaseSettings,
                                              MigrationSteps, evaluateDatabase,
                                              migrationStep, unCheckDatabase)
import           Database.Beam.Migrate.Types (executeMigration)
import           Database.Beam.Postgres

import qualified Leaderboard.Schema.V_0_0_1  as V_0_0_1 (migration)

import           Leaderboard.Schema.V_0_0_1  hiding (migration)
import           Leaderboard.Types           (LeaderboardError (..))

migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres LeaderboardDb)
migration =
  migrationStep "Initial commit" V_0_0_1.migration

leaderboardDb :: DatabaseSettings Postgres LeaderboardDb
leaderboardDb = unCheckDatabase $ evaluateDatabase migration

createSchema
  :: Connection
  -> IO (Either LeaderboardError ())
createSchema conn =
  let
    exeMigration = executeMigration runNoReturn (V_0_0_1.migration ())
    runIt = fmap Right . void $ withDatabaseDebug putStrLn conn exeMigration
    handleError = pure . Left . PostgresError
  in
    catch runIt handleError

