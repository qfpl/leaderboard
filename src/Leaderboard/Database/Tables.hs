{-# language OverloadedStrings #-}
module Leaderboard.Database.Tables where

import Opaleye
import Opaleye.Table

import Leaderboard.Types

ratingTable :: Table RatingWrite RatingRead
ratingTable =
  Table "ratings" $
  pRating Rating
    { _ratingId = pRatingId . RatingId $ optional "id"
    , _ratingValue = required "value"
    , _ratingDev = required "dev"
    , _ratingVol = required "vol"
    , _ratingInactivity = required "inactivity"
    , _ratingAge = required "age"
    }

playerTable :: Table PlayerWrite PlayerRead
playerTable =
  Table "players" $
  pPlayer Player
    { _playerId = pPlayerId . PlayerId $ optional "id"
    , _playerFirstName = required "firstName"
    , _playerLastName = optional "lastName"
    , _playerEmail = required "email"
    , _playerRating = pRatingId . RatingId $ required "ratingId"
    }

ladderTable :: Table LadderWrite LadderRead
ladderTable =
  Table "ladders" $
  pLadder Ladder
    { _ladderId = pLadderId . LadderId $ optional "id"
    , _ladderName = required "name"
    , _ladderOwner = pPlayerId . PlayerId $ required "ownerId"
    }

playerToLadderTable :: Table PlayerToLadderReadWrite PlayerToLadderReadWrite
playerToLadderTable =
  Table "playerToLadder" $
  pPlayerToLadder PlayerToLadder
    { _p2lPlayer = pPlayerId . PlayerId $ required "playerId"
    , _p2lLadder = pLadderId . LadderId $ required "ladderId"
    }
