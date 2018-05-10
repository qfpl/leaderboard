{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.TestServer where

import           Control.Exception          (Exception)
import           Control.Monad              (void)
import           Data.Foldable              (traverse_)
-- Query doesn't instance Semigroup, only Monoid
import           Data.Monoid                ((<>))
import           Database.Postgres.Temp     (StartError)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Query,
                                             connect, execute_)

data Shutdown = Shutdown deriving (Show)
instance Exception Shutdown

data DbInitError =
  PgTempStartError StartError
  | PgUrlError String
  deriving (Show)
instance Exception DbInitError

tables :: [Query]
tables =
  [ "ratings"
  , "players"
  , "ladders"
  , "playerToLadder"
  ]

truncateTables
  :: ConnectInfo
  -> IO ()
truncateTables ci@ConnectInfo{..} = do
  conn <- connect ci
  let
    qs :: [Query]
    qs = (\t -> "TRUNCATE TABLE \"" <> t <> "\"") <$> tables
  traverse_ (void . execute_ conn) qs
