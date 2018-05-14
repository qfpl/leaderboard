{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Leaderboard.SharedState where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bool              (bool)
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import           Servant.Auth.Client    (Token)
import           Servant.Client         (ClientEnv, ClientM, ServantError (..),
                                         runClientM)

import           Hedgehog               (Eq1, Gen, Show1, Var)
import qualified Hedgehog.Gen           as Gen

import           Leaderboard.Schema     (Match)

-- | Map emails to players and keep a set of admin emails
data LeaderboardState (v :: * -> *) =
  LeaderboardState (PlayerMap v) (S.Set Text) (MatchMap v)
deriving instance Show1 v => Show (LeaderboardState v)
deriving instance Eq1 v => Eq (LeaderboardState v)

type PlayerMap v = M.Map Text (PlayerWithToken v)
type MatchMap v = M.Map (Var Int v) Match

data PlayerWithToken v =
  PlayerWithToken
  { _pwtEmail    :: Text
  , _pwtName     :: Text
  , _pwtPassword :: Text
  , _pwtIsAdmin  :: Maybe Bool
  , _pwtToken    :: Var Token v
  }
  deriving (Eq, Show)

failureClient
  :: MonadIO m
  => (a -> String)
  -> ClientEnv
  -> ClientM a
  -> m ServantError
failureClient f ce a = do
  r <- liftIO $ runClientM a ce
  either pure (fail . f) r

successClient
  :: MonadIO m
  => (ServantError -> String)
  -> ClientEnv
  -> ClientM a
  -> m a
successClient f ce a = do
  r <- liftIO $ runClientM a ce
  either (fail . f) pure r

genAdminToken
  :: LeaderboardState v
  -> Maybe (Gen (Var Token v))
genAdminToken (LeaderboardState ps as _ms) =
  let
    -- Emails in admin _must_ be a subset of those in players. Without a Traversable
    -- instance for Gen I couldn't make this be not partial.
    f = _pwtToken . (M.!) ps
    mGEmail =  bool (pure . Gen.element . S.toList $ as) Nothing $ S.null as
  in
    fmap (fmap f) mGEmail
