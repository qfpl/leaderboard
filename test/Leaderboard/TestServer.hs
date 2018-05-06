{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.TestServer where

import           Control.Concurrent         (forkIO, throwTo)
import           Control.Exception          (Exception, bracket, bracket_,
                                             throw)
import           Control.Lens               ((^.))
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import           Data.Foldable              (traverse_)
-- Query doesn't instance Semigroup, only Monoid
import           Data.Monoid                ((<>))
import           Database.Postgres.Temp     (DB (..), StartError,
                                             startAndLogToTmp, stop)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Only (..), Query,
                                             connect, execute_)
import           Servant.Client             (BaseUrl (BaseUrl), ClientEnv (..),
                                             Scheme (Https))

import           Test.Tasty                 (TestTree, defaultMain, testGroup)

import           Leaderboard.Main           (doTheLeaderboard)
import           Leaderboard.Types          (ApplicationOptions (..),
                                             Command (..))

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
  , "jwk"
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
