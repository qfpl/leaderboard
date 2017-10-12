import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Monoid
import Database.Beam.Postgres.Migrate
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import Options.Applicative
import System.Environment
import System.IO.Error

import Leaderboard.Schema (migrations)

data Permanence = DryRun | Commit

data MigrateOptions
  = MigrateOptions
  { moPermanence :: Permanence
  , moWriteScript :: Maybe FilePath
  , moDbOptions :: Maybe PgMigrateOpts
  }

migrateOptionsParser :: Parser MigrateOptions
migrateOptionsParser =
  MigrateOptions <$>
  flag DryRun Commit
    (long "commit" <>
     help "Commit the migrations to the database") <*>
  optional (strOption
    (long "output" <>
     short 'o'<>
     help "Write the migration script to a file" <>
     metavar "FILEPATH")) <*>
  optional parsePgMigrateOpts

parserInfo :: ParserInfo MigrateOptions
parserInfo =
  info
    (migrateOptionsParser <**> helper)
    (progDesc "Migrate a database using beam schemata" <>
     fullDesc)

main :: IO ()
main = do
  opts <- execParser parserInfo
  password <- catch (getEnv "DB_PASS") $ \e ->
    if isDoesNotExistError e
      then pure ""
      else throw e

  case moWriteScript opts of
    Nothing -> pure ()
    Just fp -> writeMigrationScript fp migrations

  case moDbOptions opts of
    Nothing -> putStrLn "No DB credentials suppled. Exiting."
    Just creds -> do
      conn <- connect $
        (beConnectInfo creds) { connectPassword = password }
      let script = toStrict <$> migrateScript migrations
      case moPermanence opts of
        Commit -> runMigrations conn script
        DryRun -> tryMigrations conn script
      close conn

runMigrations :: Connection -> [ByteString] -> IO ()
runMigrations conn strs =
  withTransaction conn $ traverse_ (execute_ conn . Query) strs

tryMigrations :: Connection -> [ByteString] -> IO ()
tryMigrations conn strs = do
  begin conn
  traverse_ (execute_ conn . Query) strs
  rollback conn
