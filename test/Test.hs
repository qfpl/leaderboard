{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent            (forkIO, throwTo)
import           Control.Exception             (Exception, bracket, bracket_,
                                                throw)
import           Control.Lens                  ((^.))
import           Data.ByteString               (ByteString)
import           Data.ByteString.Char8         (pack)
import           Data.Semigroup                ((<>))
import           Database.Postgres.Temp        (DB (..), StartError,
                                                stop, startAndLogToTmp)
import           Database.PostgreSQL.Simple    (ConnectInfo (..))
import           Servant.Client                (BaseUrl (BaseUrl),
                                                Scheme (Https))

import           Test.Tasty                    (TestTree, defaultMain,
                                                testGroup)

import           Leaderboard.Main              (ApplicationOptions (..),
                                                Command (..),
                                                doTheLeaderboard)
import           Leaderboard.RegistrationTests (registrationTests)

data Shutdown = Shutdown deriving (Show)
instance Exception Shutdown

data DbInitError =
  PgTempStartError StartError
  | PgUrlError String
  deriving (Show)
instance Exception DbInitError

host :: String
host = "localhost"

main :: IO ()
main =
  withDb $ \DB{..} -> do
  let
    ao@ApplicationOptions{..} = ApplicationOptions connectionInfo 7645 RunApp
    url = BaseUrl Https host port ""
  withLeaderboard ao $ defaultMain (allTheTests dbConnInfo url)

allTheTests
  :: ConnectInfo
  -> BaseUrl
  -> TestTree
allTheTests ci url =
  testGroup "leaderboard"
  [ registrationTests ci url
  ]

withDb
  :: (DB -> IO a)
  -> IO a
withDb f =
  bracket
    (startAndLogToTmp [])
    (splode stop)
    (splode f)
  where
    splode f r =
      case r of
        Left e   -> throw . PgTempStartError $ e
        Right db -> f db

withLeaderboard
  :: ApplicationOptions
  -> IO a
  -> IO a
withLeaderboard ao =
  let
    aoMigrate = ao { command = MigrateDb }
    setupLeaderboard = do
      doTheLeaderboard aoMigrate
      doTheLeaderboard ao
  in
    bracket (forkIO setupLeaderboard) (`throwTo` Shutdown) . const
