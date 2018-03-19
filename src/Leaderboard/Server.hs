{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Leaderboard.Server where

import           Control.Lens
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Except        (MonadError (catchError, throwError))
import           Control.Monad.Log           (LogT (LogT), Logger, MonadLog,
                                              runLogT)
import           Control.Monad.Reader        (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Trans         (MonadTrans (lift))
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Crypto.JOSE                 (JWK)
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
  lift = LHandler . lift . LogT . const

instance MonadError ServantErr (LHandlerT env Handler) where
  throwError = lift . throwError

  -- catchError
  --   :: LHandlerT env Handler a
  --      -> (ServantErr -> LHandlerT env Handler a)
  --      -> LHandlerT env Handler a
  catchError ha te =
    _ . runReaderT $ runLHandlerT ha

instance Monad m => MonadBase IO (LHandlerT env m)

-- instance MonadBaseControl IO (LHandlerT env m) where

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

toHandler :: env -> Logger () -> (LHandlerT env Handler :~> Handler)
toHandler env l =
  NT $
    flip runLogT l .
    flip runReaderT env .
    runLHandler
