{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module Leaderboard.Server where

import Control.Lens
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.Except
import Database.Beam
import Database.Beam.Postgres
import Servant

class HasConnection env where
  connection :: Lens' env Connection

data Environment
  = Environment
  { _envConnection :: Connection
  } deriving Eq

makeLenses ''Environment

instance HasConnection Environment where
  connection = envConnection

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
