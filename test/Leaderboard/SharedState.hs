{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Leaderboard.SharedState where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import           Data.Time              (UTCTime)
import           Servant.Auth.Client    (Token)
import           Servant.Client         (ClientEnv, ClientM, ServantError (..),
                                         runClientM)

import           Hedgehog               (Concrete, Eq1,
                                         HTraversable (htraverse), MonadGen,
                                         Show1, Var (Var), concrete)
import qualified Hedgehog.Gen           as Gen

import           Leaderboard.TestClient (fromLbToken)
import           Leaderboard.Types      (RqMatch (..), RspPlayer (..))

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
type MatchMap v = M.Map (Var Int v) (TestMatch v)

data PlayerWithRsp v =
  PlayerWithRsp
  { _pwrRsp      :: Var RspPlayer v
  , _pwrEmail    :: Text
  , _pwrUsername :: Text
  , _pwrPassword :: Text
  , _pwrIsAdmin  :: Maybe Bool
  }
  deriving (Eq, Show)

instance HTraversable PlayerWithRsp where
  htraverse f PlayerWithRsp{..} =
    case _pwrRsp of
      (Var vr) -> (\_pwrRsp -> PlayerWithRsp{..}) <$> (Var <$> f vr)

data TestMatch v =
  TestMatch
  { _tmPlayer1      :: Var RspPlayer v
  , _tmPlayer2      :: Var RspPlayer v
  , _tmPlayer1Score :: Int
  , _tmPlayer2Score :: Int
  , _tmTime         :: UTCTime
  }
  deriving (Eq, Show)

instance HTraversable TestMatch where
  htraverse f TestMatch{..} =
    case (_tmPlayer1, _tmPlayer2) of
      (Var rp1, Var rp2) ->
        let fps = (,) <$> (Var <$> f rp1) <*> (Var <$> f rp2)
         in (\(_tmPlayer1, _tmPlayer2) -> TestMatch{..}) <$> fps

testToRq
  :: TestMatch Concrete
  -> RqMatch
testToRq TestMatch{..} =
  let
    _matchPlayer1 = _rspId . concrete $ _tmPlayer1
    _matchPlayer2  = _rspId . concrete $ _tmPlayer2
    _matchPlayer1Score  = _tmPlayer1Score
    _matchPlayer2Score = _tmPlayer2Score
    _matchTime = _tmTime
  in
    RqMatch{..}

clientToken
  :: PlayerWithRsp Concrete
  -> Token
clientToken PlayerWithRsp{..} =
  fromLbToken . _rspToken . concrete $ _pwrRsp

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

genAdminWithRsp
  :: MonadGen n
  => LeaderboardState v
  -> Maybe (n (PlayerWithRsp v))
genAdminWithRsp (LeaderboardState ps as _ms) =
  -- TODO ajmccluskey: be better
  -- Emails in admin _must_ be a subset of those in players. Without a Traversable
  -- instance for Gen I couldn't make this be not partial.
  if null as
  then Nothing
  else pure $ (M.!) ps <$> (Gen.element . S.toList $ as)

genPlayerWithRsp
  :: MonadGen n
  => PlayerMap v
  -> Maybe (n (PlayerWithRsp v))
genPlayerWithRsp ps =
  if null ps
  then Nothing
  else pure . fmap snd . Gen.element . M.toList $ ps
