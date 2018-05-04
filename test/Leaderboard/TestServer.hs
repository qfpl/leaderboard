{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.TestServer where

import           Control.Concurrent         (forkIO, throwTo)
import           Control.Exception          (Exception, bracket, bracket_,
                                             throw)
import           Control.Lens               ((^.))
import           Control.Monad              (void)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import           Data.Semigroup             ((<>))
import           Database.Postgres.Temp     (DB (..), StartError,
                                             startAndLogToTmp, stop)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Only (..),
                                             connect, execute)
import           Network.HTTP.Client.TLS    (newTlsManager)
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

-- | Run some IO with a fresh database and leaderboar app.
withLeaderboard
  :: ApplicationOptions
  -> (ClientEnv -> IO a)
  -> IO a
withLeaderboard ao@ApplicationOptions{..} =
  let
    setupLeaderboard = do
      dropAndCreateDb _dbConnInfo
      doTheLeaderboard $ ao { _command = MigrateDb }
      doTheLeaderboard ao
    makeEnv = do
      tlsManager <- newTlsManager
      pure . ClientEnv tlsManager $ BaseUrl Https "localhost" _port ""
  in
    bracket
      (forkIO setupLeaderboard)
      (`throwTo` Shutdown) .
      (const . (makeEnv >>=))

dropAndCreateDb
  :: ConnectInfo
  -> IO ()
dropAndCreateDb ci@ConnectInfo{..} = do
  conn <- connect ci
  void $ execute conn "DROP DATABASE IF EXISTS ?" (Only connectDatabase)
  void $ execute conn "CREATE DATABASE ?" (Only connectDatabase)
