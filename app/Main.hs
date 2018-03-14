import Control.Exception
import Control.Monad.Log
  (makeDefaultLogger, simpleTimeFormat, LogType(..), levelDebug)
import Control.Retry
import Data.Either
import Data.Monoid
import Data.Word (Word16)
import Database.PostgreSQL.Simple
import Network.OAuth.OAuth2
import Network.Wai.Handler.Warp
import Options.Applicative
import System.Environment
import URI.ByteString

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import Leaderboard.Application (leaderboard)
import Leaderboard.Server (Environment(..))

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

makeOAuth2 :: IO OAuth2
makeOAuth2 =
  OAuth2 <$>
  (T.pack <$> getEnv "OAUTH_CLIENT_ID") <*>
  (T.pack <$> getEnv "OAUTH_CLIENT_SECRET") <*>
  unsafeParseURIEnvVar "OAUTH_AUTHORIZE_ENDPOINT" <*>
  unsafeParseURIEnvVar "OAUTH_ACCESS_TOKEN_ENDPOINT" <*>
  (Just <$> unsafeParseURIEnvVar "OAUTH_CALLBACK")
  where
    unsafeParseURIEnvVar s = 
      either (Prelude.error $ s <> " does not contain a valid URI") id.
      parseURI laxURIParserOptions .
      C8.pack <$>
      getEnv s

main :: IO ()
main = do
  putStrLn "leaderboard started"
  opts <- execParser parserInfo
  password <- getEnv "DB_PASS"

  logger <-
    makeDefaultLogger simpleTimeFormat (LogStdout 4096) levelDebug ()

  oauth2 <- makeOAuth2

  let connInfo =
        ConnectInfo
        { connectHost = aoDbHost opts
        , connectPort = aoDbPort opts
        , connectUser = aoDbUser opts
        , connectPassword = password
        , connectDatabase = aoDbName opts
        }

  let mkEnv conn =
        Environment
        { _envConnection = conn
        , _envOAuth2 = oauth2
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
        run (aoPort opts) $ leaderboard (mkEnv conn) logger)
    close
