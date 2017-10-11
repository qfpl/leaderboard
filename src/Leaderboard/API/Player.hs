{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}
module Leaderboard.API.Player where

import Data.Aeson
import Data.Text
import Servant

import Leaderboard.Models.Player

newtype RegisterPlayer
  = RegisterPlayer (Player' () Text (Maybe Text) Text ())

instance FromJSON RegisterPlayer where
  parseJSON =
    withObject "RegisterPlayer" $ \v ->
      fmap RegisterPlayer $
      Player () <$>
      v .: "firstName" <*>
      v .: "lastName" <*>
      v .: "email" <*>
      pure ()

type PlayerAPI =
  "register" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] Player

playerAPI :: Server PlayerAPI
playerAPI =
  register
  where
    register :: RegisterPlayer -> Handler Player
    register (RegisterPlayer p) = do
      _
