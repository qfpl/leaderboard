{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.Main where

import           Control.Monad.Log           (LogType (..), levelDebug,
                                              makeDefaultLogger,
                                              simpleTimeFormat)
import           Control.Monad.Log.Label     (Label (Label))
import           Control.Retry               (exponentialBackoff, limitRetries,
                                              recoverAll)
import           Data.Pool                   (Pool, createPool, withResource)
import           Data.Semigroup              ((<>))
import           Data.Word                   (Word16)
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp    (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import           Options.Applicative
import           System.Environment
import           System.Exit                 (ExitCode (ExitFailure), exitWith)

import           Leaderboard.Application     (leaderboard)
import           Leaderboard.Env             (Env (Env), genJwk)
import           Leaderboard.Queries         (selectOrPersistJwk)
import           Leaderboard.Schema          (createSchema)

data ApplicationOptions
  = ApplicationOptions
  { aoDbHost  :: String
  , aoDbPort  :: Word16
  , aoDbUser  :: String
  , aoDbName  :: String
  , aoPort    :: Int
  , aoCommand :: Command
  }

data Command
  = RunApp
  | MigrateDb

applicationOptionsParser :: Parser ApplicationOptions
applicationOptionsParser =
  ApplicationOptions <$>
    dbHost <*> dbPort <*> dbUser <*> dbName <*> port <*> migrateDb
  where
    dbHost = strOption (long "db_host" <> help "Database host name" <> metavar "HOST")
    dbPort = fmap read (strOption $ long "db_port" <> help "Database port" <> metavar "DB_PORT")
    dbUser = strOption (long "db_user" <> help "Database user" <> metavar "DB_USER")
    dbName = strOption (long "db_name" <> help "Database name" <> metavar "DB_NAME")
    port = fmap read (strOption $ long "port" <> help "Webapp port" <> metavar "PORT")
    migrateDb = flag RunApp MigrateDb (long "migrate" <> help "Migrate/initialise the database")

parserInfo :: ParserInfo ApplicationOptions
parserInfo =
  info
    (applicationOptionsParser <**> helper)
    (fullDesc <> progDesc "Run the leaderboard webapp")

main :: IO ()
main = do
  putStrLn "leaderboard started"
  ao@ApplicationOptions{..} <- execParser parserInfo
  password <- getEnv "DBPASS"
  pool <- mkConnectionPool ao password
  case aoCommand of
    MigrateDb ->
      withResource pool createSchema >>=
        either print (const $ putStrLn "Migrated successfully")
    RunApp    -> runApp pool aoPort

runApp
  :: Pool Connection
  -> Int
  -> IO ()
runApp pool port = do
  logger <-
    makeDefaultLogger simpleTimeFormat (LogStdout 4096) levelDebug (Label "unlabeled")
  jwk <- withResource pool (`selectOrPersistJwk` genJwk)
  let
    tlsOpts = tlsSettings "cert.pem" "key.pem"
    warpOpts = setPort port defaultSettings
    doIt jwk' = runTLS tlsOpts warpOpts $ leaderboard (Env pool jwk') logger
    exitFail e = do
      putStrLn $ "Error with JWK: " <> show e
      exitWith . ExitFailure $ 1
  either exitFail doIt jwk

mkConnectionPool
  :: ApplicationOptions
  -> String
  -> IO (Pool Connection)
mkConnectionPool ApplicationOptions{..} pass =
  createPool
    retryingConnect
    close
    numStripes
    anHourInSeconds
    maxOpenConnections
  where
    -- docs says one stripe is enough unless in a high perf environment
    numStripes = 1
    anHourInSeconds = 3600
    maxOpenConnections = 20
    ci = ConnectInfo aoDbHost aoDbPort aoDbUser pass aoDbName
    retryingConnect =
      recoverAll
        (exponentialBackoff 200 <> limitRetries 10)
        (\_ -> putStrLn "Attempting to connect to database..." *> connect ci)
