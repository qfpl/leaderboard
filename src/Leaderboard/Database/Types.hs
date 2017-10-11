{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Leaderboard.Database.Types
  ( -- * Rating
    RatingRow
  , RatingID
  , getRatingID
  , Rating(..)
    -- ** Lenses
  , ratingID
  , ratingValue
  , ratingDev
  , ratingVol
  , ratingInactivity
  , ratingAge
  , rating
    -- * Player
  , PlayerRow
  , PlayerID
  , getPlayerID
  , Player
    -- ** Lenses
  , player
  , playerID
  , playerFirstName
  , playerLastName
  , playerEmail
  , playerRating
  )
where

import Control.Lens
import Data.Typeable
import Database.Selda

type RatingRow = RowID :*: Double :*: Double :*: Double :*: Int :*: Int

newtype RatingID = RatingID { getRatingID :: RowID }
  deriving (Eq, Show, Ord, Typeable)

rating :: Iso' RatingRow Rating
rating =
  iso
    (\(a :*: b :*: c :*: d :*: e :*: f) -> Rating (RatingID a) b c d e f)
    (\(Rating (RatingID a) b c d e f) -> a :*: b :*: c :*: d :*: e :*: f)

data Rating
  = Rating
  { _ratingID :: RatingID
  , _ratingValue :: Double
  , _ratingDev :: Double
  , _ratingVol :: Double
  , _ratingInactivity :: Int
  , _ratingAge :: Int
  }
  deriving (Eq, Show, Ord)

type PlayerRow a = RowID :*: Text :*: Maybe Text :*: Text :*: a

newtype PlayerID = PlayerID { getPlayerID :: RowID }
  deriving (Eq, Show, Ord, Typeable)

player :: Iso' (PlayerRow a) (Player a)
player =
  iso
    (\(a :*: b :*: c :*: d :*: e) -> Player (PlayerID a) b c d e)
    (\(Player (PlayerID a) b c d e) -> a :*: b :*: c :*: d :*: e)

data Player a
  = Player
  { _playerID :: PlayerID
  , _playerFirstName :: Text
  , _playerLastName :: Maybe Text
  , _playerEmail :: Text
  , _playerRating :: a
  }
  deriving (Eq, Show, Ord)

makeLenses ''Player
makeLenses ''Rating
