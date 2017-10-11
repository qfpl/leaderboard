module Leaderboard.Queries
  ( -- * Player
    insertPlayer
  )
where

import Control.Lens
import Database.PostgreSQL.Simple
import Opaleye

import Leaderboard.Models.Player

insertPlayer :: Connection -> Player -> IO Player
insertPlayer conn p =
  let
    toInsert =
      p
        & playerId .~ PlayerId' Nothing
        & playerFirstName %~ pgLazyText
        & playerLastName._Just %~ pgLazyText
        & playerEmail %~ pgLazyText
        & playerRating %~ Nothing
  in
  runInsertManyReturning conn playerTable [toInsert] _
