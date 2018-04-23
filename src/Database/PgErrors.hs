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
      | isExceptionOfType @SqlError se = toPgError se PgSqlError
      | isExceptionOfType @FormatError se = toPgError se PgFormatError

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
