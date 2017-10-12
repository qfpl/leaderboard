{-# language OverloadedStrings #-}
module Leaderboard.Schema
  ( module Leaderboard.Schema.V_0_0_1
  , migrations
  , leaderboardDb
  )
where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres

import qualified Leaderboard.Schema.V_0_0_1 as V_0_0_1 (migration)

import Leaderboard.Schema.V_0_0_1 hiding (migration)

migrations :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres LeaderboardDb)
migrations =
  migrationStep "Initial commit" V_0_0_1.migration

leaderboardDb :: DatabaseSettings Postgres LeaderboardDb
leaderboardDb = unCheckDatabase $ evaluateDatabase migrations
