{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Leaderboard.Types
  ( -- * Rating
    RatingRead
  , RatingWrite
  , RatingId
  , RatingId'(..)
  , Rating
  , Rating'(..)
    -- ** Lenses
  , ratingId
  , ratingValue
  , ratingDev
  , ratingVol
  , ratingInactivity
  , ratingAge
    -- ** Product Profunctors
  , pRatingId
  , pRating
    -- * Player
  , PlayerRead
  , PlayerWrite
  , PlayerId
  , PlayerId'(..)
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
    -- * Ladder
  , LadderRead
  , LadderWrite
  , LadderId
  , LadderId'(..)
  , Ladder
  , Ladder'(..)
    -- ** Lenses
  , ladderId
  , ladderName
  , ladderOwner
    -- ** Product Profunctors
  , pLadderId
  , pLadder
    -- * PlayerToLadder
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

import Control.Lens
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
  , _ratingValue :: b
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

makeLenses ''Player'
makeLenses ''Rating'
makeLenses ''Ladder'
makeLenses ''PlayerToLadder'
