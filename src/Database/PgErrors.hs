{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Database.PgErrors where

import           Control.Exception          (Exception, SomeException,
                                             fromException, tryJust)
import           Data.Monoid                (First (First), getFirst)
import qualified Database.PostgreSQL.Simple as Pg

data PostgresException =
    PgSqlError Pg.SqlError
  | PgFormatError Pg.FormatError
  | PgQueryError Pg.QueryError
  | PgResultError Pg.ResultError
  deriving (Eq, Show)

-- | Hide the exception type so we can throw all our constructors in a list and
-- @foldMap@ over them. Saves us some boilerplate.
data P =
  forall e. Exception e => P (e -> PostgresException)

tryJustPg
  :: IO a
  -> IO (Either PostgresException a)
tryJustPg =
  tryJust fromPgException
  where
    es = [P PgSqlError, P PgFormatError, P PgQueryError, P PgResultError]
    fromPgException se =
      getFirst . foldMap (\(P f) -> First $ toPgError se f) $ es

toPgError
  :: forall e.
     Exception e
  => SomeException
  -> (e -> PostgresException)
  -> Maybe PostgresException
toPgError se f =
  f <$> fromException se
