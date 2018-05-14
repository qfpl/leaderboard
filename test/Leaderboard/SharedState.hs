{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Leaderboard.SharedState where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import           Servant.Auth.Client    (Token)
import           Servant.Client         (ClientEnv, ClientM, ServantError (..),
                                         runClientM)

import           Hedgehog               (Eq1, MonadGen, Show1, Var)
import qualified Hedgehog.Gen           as Gen

import           Leaderboard.Schema     (PlayerId)
import           Leaderboard.Types      (RqMatch)

-- | Map emails to players and keep a set of admin emails
data LeaderboardState (v :: * -> *) =
  LeaderboardState
  {_players :: PlayerMap v
  , _admins  :: S.Set Text
  , _matches :: MatchMap v
  }
deriving instance Show1 v => Show (LeaderboardState v)
deriving instance Eq1 v => Eq (LeaderboardState v)

type PlayerMap v = M.Map Text (PlayerWithToken v)
type MatchMap v = M.Map (Var Int v) RqMatch

data PlayerWithToken =
  PlayerWithToken
  { _pwtId       :: PlayerId
  , _pwtEmail    :: Text
  , _pwtUsername :: Text
  , _pwtPassword :: Text
  , _pwtIsAdmin  :: Maybe Bool
  , _pwtToken    :: Token
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
  :: MonadGen n
  => LeaderboardState v
  -> Maybe (n (Var Token v))
genAdminToken (LeaderboardState ps as _ms) =
  -- TODO ajmccluskey: be better
  -- Emails in admin _must_ be a subset of those in players. Without a Traversable
  -- instance for Gen I couldn't make this be not partial.
  if null as
  then Nothing
  else pure $ _playerToken . (M.!) ps <$> (Gen.element . S.toList $ as)

genPlayerToken
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n (Var Token v))
genPlayerToken ps =
  if null ps
  then Nothing
  else pure . fmap (_playerToken . snd) . Gen.element . M.toList $ ps
