module Leaderboard.Models.Player
  ( -- * Database schema
    playerTable
    -- * Datatypes
  , PlayerRead
  , PlayerWrite
  , PlayerId
  , PlayerId'
  , Player
  , Player'(..)
    -- ** Lenses
  , playerId
  , playerFirstName
  , playerLastName
  , playerEmail
  , playerRating
    -- ** Product Profunctors
  , pPlayerId
  , pPlayer
  )
where

import Leaderboard.Models.Internal
