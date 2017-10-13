import Network.Wai.Handler.Warp

import Control.Exception
import Control.Retry
import Data.Monoid
import Data.Word (Word16)
import Database.PostgreSQL.Simple
import Options.Applicative
import System.Environment

import Leaderboard.Application (leaderboard)

data ApplicationOptions
  = ApplicationOptions
  { aoDbHost :: String
  , aoDbPort :: Word16
  , aoDbUser :: String
  , aoDbName :: String
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
  strOption (long "db_name" <> help "Database name" <> metavar "NAME") <*>
  fmap read
    (strOption $ long "port" <> help "Webapp port" <> metavar "PORT")

parserInfo :: ParserInfo ApplicationOptions
parserInfo =
  info
    (applicationOptionsParser <**> helper)
    (fullDesc <> progDesc "Run the leaderboard webapp")

main :: IO ()
main = do
  putStrLn "leaderboard started"
  opts <- execParser parserInfo
  password <- getEnv "DB_PASS"
  let connInfo =
        ConnectInfo
        { connectHost = aoDbHost opts
        , connectPort = aoDbPort opts
        , connectUser = aoDbUser opts
        , connectPassword = password
        , connectDatabase = aoDbName opts
        }
  bracket
    (recovering
      (constantDelay 1000)
      [ logRetries
          (\SomeException{} -> pure True)
          (\b e r -> putStrLn $ defaultLogMsg b e r)
      ]
      (const (putStrLn "Connecting to database.." *> connect connInfo)))
    (\conn -> do
        putStrLn "Connected to database."
        run (aoPort opts) $ leaderboard conn)
    close
