{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeOperators #-}
{-# language QuasiQuotes #-}
module Leaderboard.API.Player where

import Control.Lens
import Control.Monad.Log as Log
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid
import Data.Text
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Network.HTTP.Client.TLS
import Servant
import URI.ByteString.QQ

import Leaderboard.Schema
import Leaderboard.Server

data RegisterPlayer
  = GoogleRegistration
  { token :: Text
  }
  deriving Generic

instance FromJSON RegisterPlayer where

type PlayerAPI =
  "register" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] Player

data AuthResult
  = AuthResult
  { sub :: Text
  , aud :: Text
  , email :: Text
  } deriving (Generic, Show, Eq)
instance FromJSON AuthResult

