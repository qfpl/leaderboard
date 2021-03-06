{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.TestHelpers where

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

tablesToTruncate :: [Query]
tablesToTruncate =
  [ "ratings"
  , "players"
  , "ladders"
  , "playerToLadder"
  , "matches"
  ]

truncateTables
  :: ConnectInfo
  -> IO ()
truncateTables ci@ConnectInfo{..} = do
  conn <- connect ci
  let
    -- I don't believe we can use parameterised queries here as the table names
    -- don't get quoted correctly -- they get treated as string literals
    qs :: [Query]
    qs = (\t -> "TRUNCATE TABLE \"" <> t <> "\"") <$> tablesToTruncate
  traverse_ (void . execute_ conn) qs
