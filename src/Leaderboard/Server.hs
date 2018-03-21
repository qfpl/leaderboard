{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- MonadBaseControl demands this apparently :(
{-# LANGUAGE UndecidableInstances       #-}

module Leaderboard.Server where

import           Control.Lens
import           Control.Monad.Base          (MonadBase (liftBase))
import           Control.Monad.Except        (MonadError (catchError, throwError))
import           Control.Monad.Log           (LogT (LogT), Logger, MonadLog,
                                              askLogger, runLogT)
import           Control.Monad.Reader        (MonadReader, ReaderT (ReaderT), ask,
                                              runReaderT)
import           Control.Monad.Trans         (MonadTrans (lift))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (StM, liftBaseWith, restoreM),
                                              MonadTransControl (StT, liftWith, restoreT),
                                              Run, defaultLiftBaseWith,
                                              defaultRestoreM, defaultRestoreT, defaultLiftWith)

import           Crypto.JOSE                 (JWK)
import Data.Functor.Identity (Identity)
import           Database.Beam
import           Database.Beam.Postgres
import           Servant

import           Leaderboard.Env             (HasDbConnPool, withConn)

newtype LHandlerT env m a
  = LHandlerT
  { runLHandlerT
    -- :: ReaderT env (ExceptT ServantErr (LogT () IO)) a
    :: ReaderT env (LogT () m) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader env
  , MonadLog ()
  , MonadIO
  )

instance MonadTrans (LHandlerT env) where
  lift = LHandlerT . lift . lift

instance MonadError ServantErr (LHandlerT env Handler) where
  throwError = lift . throwError

  catchError lha te = do
    env <- ask
    logger <- askLogger
    ha <- liftIO . runHandler . ($$ lha) $ toHandler env logger
    either te (const lha) ha

instance MonadIO m => MonadBase IO (LHandlerT env m) where
  liftBase = liftIO

instance (MonadIO m, MonadBaseControl IO m) => MonadBaseControl IO (LHandlerT env m) where
  type StM (LHandlerT env m) a = ComposeSt (LHandlerT env) (ReaderT env m) a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

-- Run (LHandlerT env) = LHandlerT env n b -> n (StT (LHandlerT env) b)

instance MonadTransControl (LHandlerT env) where
  type StT (LHandlerT env) a = StT (ReaderT env) a
  liftWith f =
      LHandlerT $ ReaderT (\re -> LogT (\le -> f (toHandler re le $$)))
  restoreT = lift

type LServer env api m = ServerT api (LHandlerT env m)

liftQuery
  :: ( MonadBeam syntax be Connection m
     , MonadIO n
     , MonadReader env n
     , HasDbConnPool env
     , MonadBaseControl IO n
     )
  => (q -> m a)
  -> q
  -> n a
liftQuery q m =
  withConn $ \conn ->
    liftIO $ withDatabase conn (q m)

toHandler :: env -> Logger () -> (LHandlerT env m :~> m)
toHandler env l =
  NT $
    flip runLogT l .
    flip runReaderT env .
    runLHandlerT
