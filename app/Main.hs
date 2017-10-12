import Network.Wai.Handler.Warp

import Control.Exception
import Data.Monoid
import Data.Word (Word16)
import Database.PostgreSQL.Simple
import Options.Applicative
import System.Environment

import Leaderboard.Application (leaderboard)

data ApplicationOptions
  = ApplicationOptions
  { aoHost :: String
  , aoDbPort :: Word16
  , aoUser :: String
  , aoPort :: Int
  }

applicationOptionsParser :: Parser ApplicationOptions
applicationOptionsParser =
  ApplicationOptions <$>
  strOption
    (long "db_host" <>
     help "Database host name" <>
     metavar "HOST") <*>
  fmap read
    (strOption $
     long "db_port" <>
     help "Database port" <>
     metavar "DB_PORT") <*>
  strOption (long "db_user" <> help "Database user" <> metavar "USER") <*>
  fmap read
    (strOption $ long "port" <> help "Webapp port" <> metavar "PORT")

parserInfo :: ParserInfo ApplicationOptions
parserInfo =
  info
    (applicationOptionsParser <**> helper)
    (fullDesc <> progDesc "Run the leaderboard webapp")

main :: IO ()
main = do
  opts <- execParser parserInfo
  password <- getEnv "DB_PASS"
  bracket 
    (connect
      ConnectInfo
      { connectHost = aoHost opts
      , connectPort = aoDbPort opts
      , connectUser = aoUser opts
      , connectPassword = password
      , connectDatabase = "leaderboard"
      })
    (run (aoPort opts) . leaderboard)
    close
