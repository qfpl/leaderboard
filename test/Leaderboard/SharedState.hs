{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Leaderboard.SharedState where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import           Servant.Client         (ClientEnv, ClientM, ServantError (..),
                                         runClientM)

import           Hedgehog               (Eq1, MonadGen, Show1, Var)
import qualified Hedgehog.Gen           as Gen

import           Leaderboard.Types      (RqMatch, RspPlayer)

-- | Map emails to players and keep a set of admin emails
data LeaderboardState (v :: * -> *) =
  LeaderboardState
  { _players :: PlayerMap v
  , _admins  :: S.Set Text
  , _matches :: MatchMap v
  }
deriving instance Show1 v => Show (LeaderboardState v)
deriving instance Eq1 v => Eq (LeaderboardState v)

type PlayerMap v = M.Map Text (PlayerWithRsp v)
type MatchMap v = M.Map (Var Int v) RqMatch

data PlayerWithRsp v =
  PlayerWithRsp
  { _pwrRsp      :: Var RspPlayer v
  , _pwrEmail    :: Text
  , _pwrUsername :: Text
  , _pwrPassword :: Text
  , _pwrIsAdmin  :: Maybe Bool
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

genAdminRsp
  :: MonadGen n
  => LeaderboardState v
  -> Maybe (n (Var RspPlayer v))
genAdminRsp (LeaderboardState ps as _ms) =
  -- TODO ajmccluskey: be better
  -- Emails in admin _must_ be a subset of those in players. Without a Traversable
  -- instance for Gen I couldn't make this be not partial.
  if null as
  then Nothing
  else pure $ _pwrRsp . (M.!) ps <$> (Gen.element . S.toList $ as)

genPlayerRsp
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n (Var RspPlayer v))
genPlayerRsp ps =
  if null ps
  then Nothing
  else pure . fmap (_pwrRsp . snd) . Gen.element . M.toList $ ps
