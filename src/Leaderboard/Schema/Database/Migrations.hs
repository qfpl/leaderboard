{-# language DeriveGeneric #-}
module Leaderboard.Database.Migrations where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres

import Leaderboard.Models.Internal

import Leaderboard.Database.Migrations.V_0_0_1 hiding (migrations)
import qualified Leaderboard.Database.Migrations.V_0_0_1 as V_0_0_1

data LeaderboardDb f
  = LeaderboardDb
  { _leaderboardRatings :: f (TableEntity RatingT)
  , _leaderboardPlayers :: f (TableEntity PlayerT)
  , _leaderboardLadders :: f (TableEntity LadderT)
  , _leaderboardPlayerToLadder :: f (TableEntity PlayerToLadderT)
  }
  deriving Generic

instance Database LeaderboardDb

leaderboardDb :: DatabaseSettings Postgres LeaderboardDb
leaderboardDb = unCheckDatabase $ evaluateDatabase migrations

migrations :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres LeaderboardDb)
migrations = _
