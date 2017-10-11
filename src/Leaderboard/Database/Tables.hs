{-# language OverloadedStrings #-}
{-# language TypeOperators #-}
module Leaderboard.Database.Tables where

import Database.Selda

import Leaderboard.Database.Types (PlayerRow, RatingRow)

ratings :: Table RatingRow
( ratings,
    ratingID :*:
    ratingValue :*:
    ratingDev :*:
    ratingVol :*:
    ratingInactivity :*:
    ratingAge
  ) =
  tableWithSelectors "ratings" $
  autoPrimary "id" :*:
  required "rating" :*:
  required "dev" :*:
  required "vol" :*:
  required "inactivity" :*:
  required "age"

players :: Table (PlayerRow RowID)
( players,
    playerID :*:
    playerFirstName :*:
    playerLastName :*:
    playerEmail :*:
    playerRating
  ) =
  tableWithSelectors "players" $
  autoPrimary "id" :*:
  required "firstName" :*:
  optional "lastName" :*:
  unique (required "email") :*:
  fk (required "rating") (ratings, ratingID)
