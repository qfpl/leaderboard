{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Database.PgErrors where

import           Control.Exception          (Exception, SomeException,
                                             fromException, tryJust)
import           Data.Maybe                 (isJust)
import           Database.PostgreSQL.Simple (FormatError (..), SqlError (..))
import           Leaderboard.Types          (LeaderboardError (PostgresError),
                                             PostgresException (..))

pgExceptionToError
  :: IO a
  -> IO (Either LeaderboardError a)
pgExceptionToError =
  tryJust fromPgException
  where
    fromPgException se
      | isExceptionOfType @SqlError se = PostgresError . PgSqlError <$> fromException se
      | isExceptionOfType @FormatError se = PostgresError . PgFormatError <$> fromException se
  -- pure $ either (Left . PostgresError . handlePgError) Right a'
  -- where
  --   handlePgError se
  --     | isExceptionOfType @SqlError se = PgSqlError
  --     case e' of
  --       e@SqlError{..} -> PgSqlError e
  --       e@FormatError{..} -> PgFormatError e

isExceptionOfType
  :: forall e.
     Exception e
  => SomeException
  -> Bool
isExceptionOfType se =
  isJust (fromException se :: Maybe e)

toPgError
  :: forall e.
     Exception e
  => SomeException
  -> (e -> PostgresException)
  -> Maybe LeaderboardError
toPgError se f =
  PostgresError . f <$> fromException se
