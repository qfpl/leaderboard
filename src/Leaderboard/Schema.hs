{-# LANGUAGE OverloadedStrings #-}
module Leaderboard.Schema
  ( module Leaderboard.Schema.V_0_0_1
  , migration
  , leaderboardDb
  )
where

import Database.Beam
import Database.Beam.Migrate
  ( MigrationSteps, CheckedDatabaseSettings, migrationStep, unCheckDatabase
  , evaluateDatabase
  )
import Database.Beam.Postgres

import qualified Leaderboard.Schema.V_0_0_1 as V_0_0_1 (migration)

import           Leaderboard.Schema.V_0_0_1 hiding (migration)

migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres LeaderboardDb)
migration =
  migrationStep "Initial commit" V_0_0_1.migration

leaderboardDb :: DatabaseSettings Postgres LeaderboardDb
leaderboardDb = unCheckDatabase $ evaluateDatabase migration
