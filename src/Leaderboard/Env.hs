{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Leaderboard.Env where

import           Control.Lens                (re, set, view)
import           Control.Monad               (join)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Crypto.Hash                 (Digest, SHA256)
import           Crypto.JOSE                 (JWK,
                                              KeyMaterialGenParam (RSAGenParam),
                                              base64url, digest, genJWK, jwkKid,
                                              thumbprint)
import           Data.Pool                   (Pool, withResource)
import           Data.Text.Strict.Lens       (utf8)
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

genJwk
  :: IO JWK
genJwk = do
  jwk' <- genJWK (RSAGenParam (4096 `div` 8))
  let
    h = view thumbprint jwk' :: Digest SHA256
    kid = view (re (base64url . digest) . utf8) h
  pure $ set jwkKid (Just kid) jwk'

