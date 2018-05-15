{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Leaderboard.Main where

import           Control.Concurrent          (MVar, putMVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Log           (LogT, askLogger, runLogT)
import qualified Control.Monad.Log           as Log
import           Control.Monad.Log.Label     (Label (Label))
import           Control.Retry               (exponentialBackoff, limitRetries,
                                              recoverAll)
import           Data.Maybe                  (fromMaybe)
import           Data.Pool                   (Pool, createPool, withResource)
import           Data.Semigroup              ((<>))
import           Data.Text                   (pack)
import           Data.Word                   (Word16)
import           Database.PostgreSQL.Simple  (ConnectInfo (..), Connection,
                                              close, connect)
import           Network.Wai.Handler.Warp    (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import           Options.Applicative         (Parser, ParserInfo, ReadM,
                                              execParser, flag, fullDesc, help,
                                              helper, info, long, maybeReader,
                                              metavar, option, progDesc,
                                              strOption, value, (<**>))
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
  , _logLevel'          :: Log.Level
  }

fromCmdLineOpts
  :: CommandLineOptions
  -> String
  -> ApplicationOptions
fromCmdLineOpts CommandLineOptions{..} pass =
  let
    _dbConnInfo = addDbPass _dbConnInfoSansPass pass
    _logLevel = Just _logLevel'
  in
    ApplicationOptions{..}

defaultLogLevel :: Log.Level
defaultLogLevel = Log.levelWarning

commandLineOptionsParser :: Parser CommandLineOptions
commandLineOptionsParser =
  CommandLineOptions <$> cisp <*> port <*> migrateDb <*> logLevel
  where
    dbHost = strOption (long "db_host" <> help "Database host name" <> metavar "HOST")
    -- TODO ajmccluskey: use the reader stuff rather than `fmap read`
    dbPort = fmap read (strOption $ long "db_port" <> help "Database port" <> metavar "DB_PORT")
    dbUser = strOption (long "db_user" <> help "Database user" <> metavar "DB_USER")
    dbName = strOption (long "db_name" <> help "Database name" <> metavar "DB_NAME")
    cisp = ConnectInfoSansPass <$> dbHost <*> dbPort <*> dbUser <*> dbName
    port = fmap read (strOption $ long "port" <> help "Webapp port" <> metavar "PORT")
    migrateDb = flag RunApp MigrateDb (long "migrate" <> help "Migrate/initialise the database")
    logLevel = option readLogLevel (long "log_level" <> help "Log level" <> metavar "LOG_LEVEL" <> value defaultLogLevel)

readLogLevel
  :: ReadM Log.Level
readLogLevel =
  maybeReader $ \s -> case s of
    "debig"    -> Just Log.levelDebug
    "info"     -> Just Log.levelInfo
    "warning"  -> Just Log.levelWarning
    "error"    -> Just Log.levelError
    "critical" -> Just Log.levelCritical
    _          -> Nothing

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
  doTheLeaderboard Nothing $ fromCmdLineOpts co password

doTheLeaderboard
  :: Maybe (MVar ())
  -> ApplicationOptions
  -> IO ()
doTheLeaderboard ready ao@ApplicationOptions{..} = do
  let logLevel = fromMaybe defaultLogLevel _logLevel
  logger <-
    Log.makeDefaultLogger Log.simpleTimeFormat (Log.LogStdout 4096) logLevel (Label "unlabeled")
  flip runLogT logger $ do
    Log.info $ "Doing leaderboard with these options: " <> pack (show ao)
    let conn = connect _dbConnInfo
    pool <- mkConnectionPool conn
    case _command of
      MigrateDb -> liftIO $
        withResource pool createSchema >>=
          either print (const $ putStrLn "Migrated successfully")
      RunApp    -> runApp ready pool _port

addDbPass
  :: ConnectInfoSansPass
  -> String
  -> ConnectInfo
addDbPass ConnectInfoSansPass{..} pass =
  let connectPassword = pass
   in ConnectInfo{..}

runApp
  :: Maybe (MVar ())
  -> Pool Connection
  -> Int
  -> LogT Label IO ()
runApp ready pool port = do
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
    maybe (pure ()) (liftIO . (`putMVar` ())) ready
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

