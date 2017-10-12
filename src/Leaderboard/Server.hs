{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeOperators #-}
module Leaderboard.Server where

import Control.Monad.Reader
import Control.Monad.Except
import Database.Beam
import Database.Beam.Postgres
import Servant

newtype LHandler a
  = LHandler
  { runLHandler :: ReaderT Connection Handler a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader Connection
  , MonadError ServantErr
  , MonadIO
  )

type LServer api = ServerT api LHandler

liftQuery
  :: ( MonadBeam syntax be handle m
     , MonadIO n
     , MonadReader handle n
     )
  => (q -> m a)
  -> q
  -> n a
liftQuery q m = do
  conn <- ask
  liftIO $ withDatabase conn (q m)

toHandler :: Connection -> (LHandler :~> Handler)
toHandler conn =
  NT $ \lh -> runReaderT (runLHandler lh) conn
