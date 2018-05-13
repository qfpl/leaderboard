{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Leaderboard.RegistrationTests
  ( registrationTests
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bool                 (bool)
import qualified Data.Map                  as M
import           Data.Semigroup            ((<>))
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Auth.Client       (Token)
import           Servant.Client            (ClientEnv, ClientM,
                                            ServantError (..), runClientM)

import           Hedgehog                  (Callback (..), Command (Command),
                                            Eq1, Gen, HTraversable (htraverse),
                                            PropertyT, Show1, Var (Var),
                                            annotateShow, assert, concrete,
                                            executeSequential, failure, forAll,
                                            property, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import           Leaderboard.TestClient    (fromLbToken, fromLbToken',
                                            getPlayerCount, register,
                                            registerFirst)
import           Leaderboard.Types         (PlayerCount (..),
                                            RegisterPlayer (..))

data PlayerWithToken v =
  PlayerWithToken
  { _pwtEmail    :: Text
  , _pwtName     :: Text
  , _pwtPassword :: Text
  , _pwtIsAdmin  :: Maybe Bool
  , _pwtToken    :: Var Token v
  }
  deriving (Eq, Show)
-- makeLenses ''PlayerWithToken

registrationTests
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTests truncateTables env =
    testGroup "registration" [
      propRegFirst env truncateTables
    , propRegister env truncateTables
    ]

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

genNonEmptyUnicode
  :: Gen Text
genNonEmptyUnicode =
  Gen.text (Range.linear 1 100) Gen.unicode

genAdminToken
  :: RegisterState v
  -> Maybe (Gen (Var Token v))
genAdminToken (RegisterState ps as) =
  let
    -- Emails in admin _must_ be a subset of those in players. Without a Traversable
    -- instance for Gen I couldn't make this be not partial.
    f = _pwtToken . (M.!) ps
    mGEmail =  bool (pure . Gen.element . S.toList $ as) Nothing $ S.null as
  in
    fmap (fmap f) mGEmail

type PlayerMap v = M.Map Text (PlayerWithToken v)

genRegPlayerRandomAdmin
  :: PlayerMap v
  -> Gen RegisterPlayer
genRegPlayerRandomAdmin ps =
  LeaderboardRegistration
    <$> Gen.filter (`S.notMember` M.keysSet ps) genNonEmptyUnicode
    <*> genNonEmptyUnicode
    <*> genNonEmptyUnicode
    <*> Gen.maybe Gen.bool

mkPlayerWithToken
  :: RegisterPlayer
  -> Var Token v
  -> PlayerWithToken v
mkPlayerWithToken LeaderboardRegistration{..} _pwtToken =
  let
    _pwtEmail = _lbrEmail
    _pwtName = _pwtPassword
    _pwtPassword = _lbrPassword
    _pwtIsAdmin = _lbrIsAdmin
  in
    PlayerWithToken{..}

-- | Map emails to players and keep a set of admin emails
data RegisterState (v :: * -> *) =
  RegisterState (PlayerMap v) (S.Set Text)

--------------------------------------------------------------------------------
-- PLAYER COUNT
--------------------------------------------------------------------------------

data GetPlayerCount (v :: * -> *) =
    GetPlayerCount
  deriving (Eq, Show)
instance HTraversable GetPlayerCount where
  htraverse _ _ = pure GetPlayerCount

cGetPlayerCount
  :: ClientEnv
  -> Command Gen (PropertyT IO) RegisterState
cGetPlayerCount env =
  let
    gen _s = Just . pure $ GetPlayerCount
    exe _i = successClient show env $ unPlayerCount <$> getPlayerCount
  in
    Command gen exe [
      Ensure $ \(RegisterState ps as) _sNew _i c -> do
        annotateShow ps
        annotateShow as
        length ps === fromIntegral c
        assert $ length ps >= length as
    ]

--------------------------------------------------------------------------------
-- REGISTER FIRST
--------------------------------------------------------------------------------
deriving instance Show1 v => Show (RegisterState v)
deriving instance Eq1 v => Eq (RegisterState v)

newtype RegFirst (v :: * -> *) =
    RegFirst RegisterPlayer
  deriving (Eq, Show)
instance HTraversable RegFirst where
  htraverse _ (RegFirst rp)          = pure (RegFirst rp)

newtype RegFirstForbidden (v :: * -> *) =
  RegFirstForbidden RegisterPlayer
  deriving (Eq, Show)
instance HTraversable RegFirstForbidden where
  htraverse _ (RegFirstForbidden rp) = pure (RegFirstForbidden rp)

cRegFirst
  :: ClientEnv
  -> Command Gen (PropertyT IO) RegisterState
cRegFirst env =
  let
    gen (RegisterState ps _as) =
      bool Nothing (Just $ RegFirst <$> genRegPlayerRandomAdmin ps) $ null ps
    execute (RegFirst rp) =
      let mkError = (("Should succeed with token, but got: " <>) . show)
       in successClient mkError env . fmap fromLbToken' $ registerFirst rp
  in
    Command gen execute [
      Require $ \(RegisterState ps _) _input -> null ps
    , Update $ \_s (RegFirst lbr@LeaderboardRegistration{..}) t ->
        RegisterState (M.singleton _lbrEmail (mkPlayerWithToken lbr t)) (S.singleton _lbrEmail)
    , Ensure $ \_sOld (RegisterState psNew _) (RegFirst _rp) _t -> length psNew === 1
    ]

cRegFirstForbidden
  :: ClientEnv
  -> Command Gen (PropertyT IO) RegisterState
cRegFirstForbidden env =
  let
    gen (RegisterState ps _as) =
      bool (Just $ RegFirstForbidden <$> genRegPlayerRandomAdmin ps) Nothing $ null ps
    execute (RegFirstForbidden rp) =
      failureClient (const "Should return 403") env $ registerFirst rp
  in
    Command gen execute [
      Require $ \(RegisterState ps _as) _input -> not (null ps)
    , Ensure $ \sOld sNew _input se -> do
        sOld === sNew
        case se of
          FailureResponse{..} -> responseStatus === forbidden403
          _                   -> failure
    ]

--------------------------------------------------------------------------------
-- REGISTER
--------------------------------------------------------------------------------

data Register (v :: * -> *) =
  Register RegisterPlayer (Var Token v)
  deriving (Eq, Show)
instance HTraversable Register where
  htraverse f (Register rp (Var gt)) = Register rp . Var <$> f gt

cRegister
  :: ClientEnv
  -> Command Gen (PropertyT IO) RegisterState
cRegister env =
  let
    gen rs@(RegisterState ps _as) =
      (Register <$> genRegPlayerRandomAdmin ps <*>) <$> genAdminToken rs
    execute (Register rp token) =
      successClient show env . fmap fromLbToken $ register (concrete token) rp
  in
    Command gen execute [
      Require $ \(RegisterState _ps as) _input -> not (null as)
    , Update $ \(RegisterState ps as) (Register rp@LeaderboardRegistration{..} _rqToken) t ->
        let
          newPlayers = M.insert _lbrEmail (mkPlayerWithToken rp t) ps
          newAdmins =
            case _lbrIsAdmin of
              Just True -> S.insert _lbrEmail as
              _         -> as
        in
          RegisterState newPlayers newAdmins
    , Ensure $ \(RegisterState psOld _asOld) (RegisterState psNew _asNew) _input _output ->
        length psNew === length psOld + 1
    ]

initialState
  :: RegisterState (v :: * -> *)
initialState =
  RegisterState M.empty S.empty

propRegFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegFirst env truncateTables =
  testProperty "register-first" . property $ do
  liftIO truncateTables
  let cs = ($ env) <$> [cRegFirst, cGetPlayerCount, cRegFirstForbidden]
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState cs
  executeSequential initialState commands

propRegister
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegister env truncateTables =
  testProperty "register-counts" . property $ do
  liftIO truncateTables
  let cs = ($ env) <$> [cRegister, cRegFirst, cRegFirstForbidden, cGetPlayerCount]
  commands <- forAll $
    Gen.sequential (Range.linear 1 100) initialState cs
  executeSequential initialState commands

