{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language RecordWildCards #-}
{-# language TypeOperators #-}
module Leaderboard.API.Player where

import Control.Lens
import Data.Aeson
import Data.Text
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Servant

import Leaderboard.Database
import Leaderboard.Models.Player
import Leaderboard.Server

data RegisterPlayer
  = RegisterPlayer
  { firstName :: Text
  , lastName :: Maybe Text
  , email :: Text
  }
  deriving Generic

instance FromJSON RegisterPlayer where

type PlayerAPI =
  "register" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] Player

playerAPI :: LServer PlayerAPI
playerAPI =
  register
  where
    register :: RegisterPlayer -> LHandler Player
    register RegisterPlayer{..} = do
      [p] <- liftQuery
        (runInsertReturningList $ leaderboardDb ^. leaderboardPlayers)
        (insertValues [Player (Auto Nothing) firstName lastName email])
      pure p
