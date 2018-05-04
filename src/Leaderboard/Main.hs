{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Leaderboard.Main where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Log           (LogT, askLogger, runLogT)
import qualified Control.Monad.Log           as Log
import           Control.Monad.Log.Label     (Label (Label))
import           Control.Retry               (exponentialBackoff, limitRetries,
                                              recoverAll)
import           Data.Pool                   (Pool, createPool, withResource)
import           Data.Semigroup              ((<>))
import           Data.Text                   (pack)
import           Data.Word                   (Word16)
import           Database.PostgreSQL.Simple  (ConnectInfo (..), Connection,
                                              close, connect)
import           Network.Wai.Handler.Warp    (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import           Options.Applicative         (Parser, ParserInfo, flag,
                                              fullDesc, help, helper, info,
                                              long, metavar, progDesc,
                                              strOption, (<**>), execParser)
import           System.Environment
import           System.Exit                 (ExitCode (ExitFailure), exitWith)

import           Leaderboard.Application     (leaderboard)
import           Leaderboard.Env             (Env (Env), genJwk)
import           Leaderboard.Queries         (selectOrPersistJwk)
import           Leaderboard.Schema          (createSchema)
import           Leaderboard.Types           (ApplicationOptions (..),
                                              Command (..))

data ConnectInfoSansPass
  = ConnectInfoSansPass
  { connectHost     :: String
  , connectPort     :: Word16
  , connectUser     :: String
  , connectDatabase :: String
  }
  deriving (Eq, Show)

data CommandLineOptions
  = CommandLineOptions
  { _dbConnInfoSansPass :: ConnectInfoSansPass
  , _port               :: Int
  , _command            :: Command
  }

fromCmdLineOpts
  :: CommandLineOptions
  -> String
  -> ApplicationOptions
fromCmdLineOpts CommandLineOptions{..} pass =
  let _dbConnInfo = addDbPass _dbConnInfoSansPass pass
   in ApplicationOptions{..}

commandLineOptionsParser :: Parser CommandLineOptions
commandLineOptionsParser =
  CommandLineOptions <$> cisp <*> port <*> migrateDb
  where
    dbHost = strOption (long "db_host" <> help "Database host name" <> metavar "HOST")
    dbPort = fmap read (strOption $ long "db_port" <> help "Database port" <> metavar "DB_PORT")
    dbUser = strOption (long "db_user" <> help "Database user" <> metavar "DB_USER")
    dbName = strOption (long "db_name" <> help "Database name" <> metavar "DB_NAME")
    cisp = ConnectInfoSansPass <$> dbHost <*> dbPort <*> dbUser <*> dbName
    port = fmap read (strOption $ long "port" <> help "Webapp port" <> metavar "PORT")
    migrateDb = flag RunApp MigrateDb (long "migrate" <> help "Migrate/initialise the database")

parserInfo :: ParserInfo CommandLineOptions
parserInfo =
  info
    (commandLineOptionsParser <**> helper)
    (fullDesc <> progDesc "Run the leaderboard webapp")

main :: IO ()
main = do
  co <- execParser parserInfo
  -- TODO ajmccluskey: we might not need a pass, so use the maybe version of this and blank pass on Nothing
  password <- getEnv "DBPASS"
  doTheLeaderboard $ fromCmdLineOpts co password

doTheLeaderboard
  :: ApplicationOptions
  -> IO ()
doTheLeaderboard ApplicationOptions{..} = do
  logger <-
    Log.makeDefaultLogger Log.simpleTimeFormat (Log.LogStdout 4096) Log.levelDebug (Label "unlabeled")
  flip runLogT logger $ do
    let conn = connect _dbConnInfo
    pool <- mkConnectionPool conn
    case _command of
      MigrateDb -> liftIO $
        withResource pool createSchema >>=
          either print (const $ putStrLn "Migrated successfully")
      RunApp    -> runApp pool _port

addDbPass
  :: ConnectInfoSansPass
  -> String
  -> ConnectInfo
addDbPass ConnectInfoSansPass{..} pass =
  let connectPassword = pass
   in ConnectInfo{..}

runApp
  :: Pool Connection
  -> Int
  -> LogT Label IO ()
runApp pool port = do
    logger <- askLogger
    Log.info "Creating/retrieving JWK"
    jwk <- liftIO $ withResource pool (`selectOrPersistJwk` genJwk)
    let
      tlsOpts = tlsSettings "cert.pem" "key.pem"
      warpOpts = setPort port defaultSettings
      doIt jwk' = liftIO . runTLS tlsOpts warpOpts $ leaderboard (Env pool jwk') logger
      exitFail e = do
        Log.error $ "Error with JWK: " <> (pack . show $ e)
        liftIO . exitWith . ExitFailure $ 1
    Log.info $ "Starting app on port " <> (pack . show $ port)
    either exitFail doIt jwk

mkConnectionPool
  :: IO Connection
  -> LogT env IO (Pool Connection)
mkConnectionPool ci = do
  logger <- askLogger
  liftIO $
    createPool
      (retryingConnect logger)
      close
      numStripes
      anHourInSeconds
      maxOpenConnections
  where
    -- docs says one stripe is enough unless in a high perf environment
    numStripes = 1
    anHourInSeconds = 3600
    maxOpenConnections = 20
    retryingConnect l =
      recoverAll
        (exponentialBackoff 200 <> limitRetries 10)
        (\_ -> runLogT (Log.info "Attempting to connect to database...") l *> liftIO ci)

