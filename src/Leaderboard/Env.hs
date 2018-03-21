{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Leaderboard.Env where

import           Control.Monad               (join)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Crypto.JOSE                 (JWK)
import           Data.Pool                   (Pool, withResource)
import           Database.PostgreSQL.Simple  (Connection)

data Env =
  Env
  { _envDbConnPool :: Pool Connection
  , _envJWK        :: JWK
  }

class HasJWK env where
  jwk :: env -> JWK

instance HasJWK Env where
  jwk = _envJWK

class HasDbConnPool a where
  dbConnPool :: a -> Pool Connection

instance HasDbConnPool Env where
  dbConnPool = _envDbConnPool

withConn
  :: ( MonadBaseControl IO m
     , MonadReader r m
     , HasDbConnPool r
     )
  => (Connection -> m a)
  -> m a
withConn f =
  join . asks $ (`withResource` f) . dbConnPool
