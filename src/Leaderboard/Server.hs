{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Leaderboard.Server where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Log
import           Control.Monad.Reader
import           Crypto.JOSE            (JWK)
import           Database.Beam
import           Database.Beam.Postgres
import           Servant

class HasConnection env where
  connection :: Lens' env Connection

class HasJWK env where
  jwk :: Lens' env JWK

data Environment
  = Environment
  { _envConnection :: Connection
  , _envJWK        :: JWK
  } deriving Eq

makeLenses ''Environment

instance HasConnection Environment where
  connection = envConnection

instance HasJWK Environment where
  jwk = envJWK

newtype LHandler env a
  = LHandler
  { runLHandler
    :: ReaderT env (ExceptT ServantErr (LogT () IO)) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader env
  , MonadError ServantErr
  , MonadLog ()
  , MonadIO
  )

type LServer env api = ServerT api (LHandler env)

liftQuery
  :: ( MonadBeam syntax be Connection m
     , MonadIO n
     , MonadReader env n
     , HasConnection env
     )
  => (q -> m a)
  -> q
  -> n a
liftQuery q m = do
  conn <- view connection
  liftIO $ withDatabase conn (q m)

toHandler :: env -> Logger () -> (LHandler env :~> Handler)
toHandler env l =
  NT $
    Handler . ExceptT .
    flip runLogT l .
    runExceptT .
    flip runReaderT env .
    runLHandler
