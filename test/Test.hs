{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent             (forkIO, throwTo)
import           Control.Exception              (Exception, bracket, bracket_,
                                                 throw)
import           Control.Lens                   ((^.))
import           Data.ByteString.Char8          (pack)
import           Data.Semigroup                 ((<>))
import           Database.Postgres.Temp         (DB (..), StartError,
                                                 startLocalhost, stop)
import           Database.PostgreSQL.Simple     (ConnectInfo (..))
import           Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import           Servant.Client                 (BaseUrl (BaseUrl),
                                                 Scheme (Https))

import           Test.Tasty                     (TestTree, defaultMain,
                                                 testGroup)

import           Leaderboard.Main               (ApplicationOptions (..), aoPort,
                                                 Command (RunApp),
                                                 mkConnectionPool, runApp)
import           Leaderboard.RegistrationTests  (registrationTests)

data Shutdown = Shutdown deriving (Show)
instance Exception Shutdown

data DbInitError =
  PgTempStartError StartError
  | PgUrlError String
  deriving (Show)
instance Exception DbInitError

appOptions
  :: ConnectInfo
  -> ApplicationOptions
appOptions ci =
  ApplicationOptions ci 7645 RunApp

host :: String
host = "localhost"

main :: IO ()
main =
  withDb $ \DB{..} -> do
  let
    cs = pack connectionString
    mCi = parseDatabaseUrl connectionString
    parseSplode = throw . PgUrlError $ "Failed to parse connection string: " <> connectionString
  ci <- maybe parseSplode pure mCi
  let
    ao = appOptions ci
    url = BaseUrl Https host (ao ^. aoPort) ""
  withLeaderboard ci $ defaultMain (allTheTests url)

allTheTests
  :: BaseUrl
  -> TestTree
allTheTests url =
  testGroup "leaderboard"
  [ registrationTests url
  ]

withDb
  :: (DB -> IO a)
  -> IO a
withDb f =
  bracket
    (startLocalhost [])
    (splode stop)
    (splode f)
  where
    splode f r =
      case r of
        Left e   -> throw . PgTempStartError $ e
        Right db -> f db

withLeaderboard
  :: ConnectInfo
  -> IO a
  -> IO a
withLeaderboard ci =
  bracket (forkIO $ startLeaderboard ci) (`throwTo` Shutdown) . const

startLeaderboard
  :: ConnectInfo
  -> IO ()
startLeaderboard ci = do
  pool <- mkConnectionPool ci
  runApp pool (fromIntegral . connectPort $ ci)
