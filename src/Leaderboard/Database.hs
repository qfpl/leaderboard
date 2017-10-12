{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
module Leaderboard.Database where

import Control.Lens (makeLenses)
import Database.Beam

import Leaderboard.Models.Internal

data LeaderboardDb f
  = LeaderboardDb
  { _leaderboardRatings :: f (TableEntity RatingT)
  , _leaderboardPlayers :: f (TableEntity PlayerT)
  , _leaderboardLadders :: f (TableEntity LadderT)
  , _leaderboardPlayerToLadder :: f (TableEntity PlayerToLadderT)
  }
  deriving Generic

instance Database LeaderboardDb

leaderboardDb :: DatabaseSettings backend LeaderboardDb
leaderboardDb = defaultDbSettings

makeLenses ''LeaderboardDb
