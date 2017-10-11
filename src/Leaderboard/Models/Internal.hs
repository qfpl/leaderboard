{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Leaderboard.Models.Internal where

import Data.Aeson
import Control.Lens (makeLenses)
import Data.Profunctor.Product.TH
import Data.Text
import Opaleye

newtype RatingId' a = RatingId { getRatingId :: a }
  deriving (Eq, Show, Ord)

type RatingId = RatingId' Int

$(makeAdaptorAndInstance "pRatingId" ''RatingId')

data Rating' a b c d e f
  = Rating
  { _ratingId :: a
  , _ratingRating :: b
  , _ratingDev :: c
  , _ratingVol :: d
  , _ratingInactivity :: e
  , _ratingAge :: f
  }
  deriving (Eq, Show, Ord)

$(makeAdaptorAndInstance "pRating" ''Rating')

type RatingRead =
  Rating'
    (RatingId' (Column PGInt4))
    (Column PGFloat8)
    (Column PGFloat8)
    (Column PGFloat8)
    (Column PGInt4)
    (Column PGInt4)

type RatingWrite =
  Rating'
    (RatingId' (Maybe (Column PGInt4)))
    (Column PGFloat8)
    (Column PGFloat8)
    (Column PGFloat8)
    (Column PGInt4)
    (Column PGInt4)

type Rating = Rating' RatingId Double Double Double Int Int


newtype PlayerId' a = PlayerId { getPlayerId :: a }
  deriving (Eq, Show, Ord)

type PlayerId = PlayerId' Int

$(makeAdaptorAndInstance "pPlayerId" ''PlayerId')

data Player' a b c d e
  = Player
  { _playerId :: a
  , _playerFirstName :: b
  , _playerLastName :: c
  , _playerEmail :: d
  , _playerRating :: e
  }
  deriving (Eq, Show, Ord)

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) =>
  ToJSON (Player' a b c d e) where
  toJSON (Player a b c d e) =
    object
      [ "id" .= a
      , "firstName" .= b
      , "lastName" .= c
      , "email" .= d
      , "rating" .= e
      ]

type PlayerRead =
  Player'
    (PlayerId' (Column PGInt4))
    (Column PGText)
    (Column (Nullable PGText))
    (Column PGText)
    (RatingId' (Column PGInt4))

type PlayerWrite =
  Player'
    (PlayerId' (Maybe (Column PGInt4)))
    (Column PGText)
    (Maybe (Column (Nullable PGText)))
    (Column PGText)
    (RatingId' (Column PGInt4))

type Player = Player' PlayerId Text (Maybe Text) Text RatingId

$(makeAdaptorAndInstance "pPlayer" ''Player')


newtype LadderId' a = LadderId { getLadderId :: a }
  deriving (Eq, Show, Ord)

type LadderId = LadderId' Int

$(makeAdaptorAndInstance "pLadderId" ''LadderId')

data Ladder' a b c
  = Ladder
  { _ladderId :: a
  , _ladderName :: b
  , _ladderOwner :: c
  }
  deriving (Eq, Show, Ord)

type LadderRead =
  Ladder'
    (LadderId' (Column PGInt4))
    (Column PGText)
    (PlayerId' (Column PGInt4))

type LadderWrite =
  Ladder'
    (LadderId' (Maybe (Column PGInt4)))
    (Column PGText)
    (PlayerId' (Column PGInt4))

type Ladder = Ladder' LadderId Text PlayerId

$(makeAdaptorAndInstance "pLadder" ''Ladder')


data PlayerToLadder' a b
  = PlayerToLadder
  { _p2lPlayer :: a
  , _p2lLadder :: b
  }
  deriving (Eq, Show, Ord)

type PlayerToLadderReadWrite =
  PlayerToLadder'
    (PlayerId' (Column PGInt4))
    (LadderId' (Column PGInt4))

type PlayerToLadder = PlayerToLadder' PlayerId LadderId 

$(makeAdaptorAndInstance "pPlayerToLadder" ''PlayerToLadder')

ratingTable :: Table RatingWrite RatingRead
ratingTable =
  Table "ratings" $
  pRating Rating
    { _ratingId = pRatingId . RatingId $ optional "id"
    , _ratingRating = required "value"
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
    , _playerRating = pRatingId . RatingId $ required "rating"
    }

ladderTable :: Table LadderWrite LadderRead
ladderTable =
  Table "ladders" $
  pLadder Ladder
    { _ladderId = pLadderId . LadderId $ optional "id"
    , _ladderName = required "name"
    , _ladderOwner = pPlayerId . PlayerId $ required "owner"
    }

playerToLadderTable :: Table PlayerToLadderReadWrite PlayerToLadderReadWrite
playerToLadderTable =
  Table "playerToLadder" $
  pPlayerToLadder PlayerToLadder
    { _p2lPlayer = pPlayerId . PlayerId $ required "player"
    , _p2lLadder = pLadderId . LadderId $ required "ladder"
    }

makeLenses ''Player'
makeLenses ''Rating'
makeLenses ''Ladder'
makeLenses ''PlayerToLadder'
