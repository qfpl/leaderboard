module Leaderboard.Models.PlayerToLadder
  ( -- * PlayerToLadder
    -- ** Database schema
    playerToLadderTable
    -- ** Datatypes
  , PlayerToLadderReadWrite
  , PlayerToLadder
  , PlayerToLadder'(..)
    -- ** Lenses
  , p2lPlayer
  , p2lLadder
    -- ** Product Profunctors
  , pPlayerToLadder
  )
where

import Leaderboard.Models.Internal
