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
import           Data.Semigroup             ((<>))
import           Database.PostgreSQL.Simple (ConnectInfo (..), Only (..),
                                             connect, execute)
import           Database.Postgres.Temp     (DB (..), StartError,
                                             startAndLogToTmp, stop)
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

-- | Run some MonadIO with a fresh database and leaderboar app.
withLeaderboard
  :: MonadIO m
  => ApplicationOptions
  -> (ClientEnv -> m a)
  -> m a
withLeaderboard ao@ApplicationOptions{..} action = do
  let
    setupLeaderboard = do
      dropAndCreateDb _dbConnInfo
      doTheLeaderboard $ ao { _command = MigrateDb }
      doTheLeaderboard ao
    makeEnv = do
      tlsManager <- newTlsManager
      pure . ClientEnv tlsManager $ BaseUrl Https "localhost" _port ""
  -- TODO ajmccluskey: we should probs do something about exceptions here
  threadId <- liftIO $ forkIO setupLeaderboard
  result <- makeEnv >>= action
  liftIO $ throwTo threadId Shutdown
  pure result

dropAndCreateDb
  :: ConnectInfo
  -> IO ()
dropAndCreateDb ci@ConnectInfo{..} = do
  conn <- connect ci
  void $ execute conn "DROP DATABASE IF EXISTS ?" (Only connectDatabase)
  void $ execute conn "CREATE DATABASE ?" (Only connectDatabase)
