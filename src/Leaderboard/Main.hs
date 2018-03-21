{-# LANGUAGE RecordWildCards #-}

module Leaderboard.Main where

import           Control.Exception
import           Control.Monad.Log          (LogType (..), levelDebug,
                                             makeDefaultLogger,
                                             simpleTimeFormat)
import           Control.Retry              (constantDelay, defaultLogMsg,
                                             logRetries, recovering, recoverAll, exponentialBackoff, limitRetries)
import           Data.Either
import           Data.Monoid
import           Data.Pool                  (Pool, createPool, withResource)
import           Data.Word                  (Word16)
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           System.Environment
import           URI.ByteString

import qualified Data.ByteString.Char8      as C8
import qualified Data.Text                  as T

import           Leaderboard.Application    (leaderboard)
import           Leaderboard.Env            (Env (Env))
import           Leaderboard.Queries        (getOrCreateJwk)

data ApplicationOptions
  = ApplicationOptions
  { aoDbHost :: String
  , aoDbPort :: Word16
  , aoDbUser :: String
  , aoDbName :: String
  , aoPort   :: Int
  }

applicationOptionsParser :: Parser ApplicationOptions
applicationOptionsParser =
  ApplicationOptions <$>
    dbHost <*> dbPort <*> dbUser <*> dbName <*> port
  where
    dbHost = strOption (long "db_host" <> help "Database host name" <> metavar "HOST")
    dbPort = fmap read (strOption $ long "db_port" <> help "Database port" <> metavar "DB_PORT")
    dbUser = strOption (long "db_user" <> help "Database user" <> metavar "DB_USER")
    dbName = strOption (long "db_name" <> help "Database name" <> metavar "DB_NAME")
    port = fmap read (strOption $ long "port" <> help "Webapp port" <> metavar "PORT")

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
  logger <-
    makeDefaultLogger simpleTimeFormat (LogStdout 4096) levelDebug ()
  pool <- mkConnectionPool ao password
  jwk <- withResource pool getOrCreateJwk
  run aoPort $ leaderboard (Env pool jwk) logger

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
    info = ConnectInfo aoDbHost aoDbPort aoDbUser pass aoDbName
    retryingConnect =
      recoverAll
        (exponentialBackoff 200 <> limitRetries 10)
        (\_ -> putStrLn "Attempting to connect to database..." *> connect info)
