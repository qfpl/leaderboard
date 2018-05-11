{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Database.PgErrors where

import           Control.Exception          (Exception, SomeException,
                                             fromException, tryJust)
import           Data.Maybe                 (isJust)
import           Database.PostgreSQL.Simple (FormatError (..), SqlError (..))

data PostgresException =
    PgSqlError SqlError
  | PgFormatError FormatError
  deriving (Eq, Show)

tryJustPg
  :: IO a
  -> IO (Either PostgresException a)
tryJustPg =
  tryJust fromPgException
  where
    fromPgException se
      | isExceptionOfType @SqlError se = toPgError se PgSqlError
      | isExceptionOfType @FormatError se = toPgError se PgFormatError
      | otherwise = Nothing

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
  -> Maybe PostgresException
toPgError se f =
  f <$> fromException se
