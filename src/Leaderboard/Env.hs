{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Leaderboard.Env where

import           Control.Lens                (re, set, view)
import           Control.Monad               (join)
import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.Log           (MonadLog)
import           Control.Monad.Log.Label     (Label (Label), withLabel)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Crypto.Hash                 (Digest, SHA256)
import           Crypto.JOSE                 (JWK,
                                              KeyMaterialGenParam (RSAGenParam),
                                              base64url, digest, genJWK, jwkKid,
                                              thumbprint)
import           Data.Pool                   (Pool, withResource)
import           Data.Text                   (Text)
import           Data.Text.Strict.Lens       (utf8)
import           Database.PostgreSQL.Simple  (Connection)
import           Servant                     (ServantErr, err401, errBody)
import           Servant.Auth.Server         (AuthResult (..))

import           Leaderboard.Types           (PlayerSession (..))

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

withAuthConnAndLog
  :: ( MonadError ServantErr m
     , MonadLog Label m
     , HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     )
  => AuthResult PlayerSession
  -> Text
  -> (Int -> Connection -> m a)
  -> m a
withAuthConnAndLog arp label f =
  asPlayer arp $ \pId ->
  withLabel (Label label) $
  withConn $ \conn -> f pId conn

genJwk
  :: IO JWK
genJwk = do
  jwk' <- genJWK (RSAGenParam (4096 `div` 8))
  let
    h = view thumbprint jwk' :: Digest SHA256
    kid = view (re (base64url . digest) . utf8) h
  pure $ set jwkKid (Just kid) jwk'

asPlayer
  :: MonadError ServantErr m
  => AuthResult PlayerSession
  -> (Int -> m a)
  -> m a
asPlayer arp f =
  case arp of
    Authenticated psId -> f $ _psId psId
    BadPassword -> throwError err401
    NoSuchUser -> throwError err401
    Indefinite -> throwError $ err401 { errBody = "No valid auth method found" }

