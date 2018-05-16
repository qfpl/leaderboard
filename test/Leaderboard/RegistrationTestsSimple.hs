{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.RegistrationTestsSimple
  ( registrationTestsSimple
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bool                 (bool)
import qualified Data.ByteString           as BS
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Data.Semigroup            ((<>))
import qualified Data.Set                  as S
import           Database.Beam             (Auto (Auto))
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            GenT, HTraversable (htraverse),
                                            MonadGen, MonadTest, PropertyT,
                                            Var (Var), annotateShow, assert,
                                            concrete, executeSequential,
                                            failure, forAll, property, success,
                                            test, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import           Leaderboard.Schema        (PlayerT (..))
import qualified Leaderboard.Schema        as LS
import           Leaderboard.SharedState   (PlayerMap, PlayerWithRsp (..),
                                            checkCommands, clientToken,
                                            emptyState, failureClient,
                                            genAdminWithRsp, genPlayerWithRsp,
                                            successClient)
import           Leaderboard.TestClient    (getPlayerCount, me, register,
                                            registerFirst)
import           Leaderboard.Types         (PlayerCount (..),
                                            RegisterPlayer (..),
                                            ResponsePlayer (..), Token (..))

registrationTestsSimple
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTestsSimple truncateTables env =
    testGroup "registration-simple" [
      propRegisterFirst env truncateTables
    ]

genRegPlayerRandomAdmin
  :: MonadGen n
  => n RegisterPlayer
genRegPlayerRandomAdmin =
  let
    genNonEmptyAlphaNum =
      Gen.text (Range.linear 1 20) Gen.alphaNum
  in
    LeaderboardRegistration
      <$> genNonEmptyAlphaNum
      <*> genNonEmptyAlphaNum
      <*> genNonEmptyAlphaNum
      <*> Gen.maybe Gen.bool

mkPlayerWithRsp
  :: RegisterPlayer
  -> Var ResponsePlayer v
  -> PlayerWithRsp v
mkPlayerWithRsp LeaderboardRegistration{..} rsp =
  let
    _pwrRsp = rsp
    _pwrEmail = _lbrEmail
    _pwrUsername = _lbrUsername
    _pwrPassword = _lbrPassword
    _pwrIsAdmin = _lbrIsAdmin
  in
    PlayerWithRsp{..}

newtype SimpleState (v :: * -> *) =
  SimpleState Bool
instance HTraversable SimpleState where
  htraverse _ (SimpleState n) = pure (SimpleState n)

--------------------------------------------------------------------------------
-- REGISTER FIRST
--------------------------------------------------------------------------------

newtype RegFirst (v :: * -> *) =
    RegFirst RegisterPlayer
  deriving (Eq, Show)
instance HTraversable RegFirst where
  htraverse _ (RegFirst rp) = pure (RegFirst rp)

newtype RegFirstForbidden (v :: * -> *) =
  RegFirstForbidden RegisterPlayer
  deriving (Eq, Show)
instance HTraversable RegFirstForbidden where
  htraverse _ (RegFirstForbidden rp) = pure (RegFirstForbidden rp)

cRegisterFirst
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) SimpleState
cRegisterFirst env =
  let
    gen (SimpleState registeredFirst) =
      if registeredFirst
      then Nothing
      else Just (RegFirst <$> genRegPlayerRandomAdmin)
    execute (RegFirst rp) =
      let mkError = ("Error registering first user: " <>) . show
       in successClient mkError env . registerFirst $ rp
  in
    Command gen execute [
      Require $ \(SimpleState registeredFirst) _input -> not registeredFirst
    , Update $ \_oldState _regFirst _output -> SimpleState True
    , Ensure $ \(SimpleState rOld) (SimpleState rNew) _input
                (ResponsePlayer (LS.PlayerId (Auto mid)) (Token t)) -> do
        rOld === False
        rNew === True
        assert $ maybe False (>= 0) mid
        assert $ not (BS.null t)
    ]

cRegisterFirstForbidden
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) SimpleState
cRegisterFirstForbidden env =
  let
    gen (SimpleState registeredFirst) =
      if registeredFirst
      then Just (RegFirstForbidden <$> genRegPlayerRandomAdmin)
      else Nothing
    execute (RegFirstForbidden rp) =
      failureClient (const "Should fail with 403") env $ registerFirst rp
  in
    Command gen execute [
      Require $ \(SimpleState registeredFirst) _input -> registeredFirst
      -- State shouldn't change, so no `Update`
    , Ensure $ \_sOld _sNew _input se ->
        case se of
          FailureResponse{..} -> responseStatus === forbidden403
          _                   -> failure
    ]

propRegisterFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegisterFirst env reset =
  testProperty "register-first" . property $ do
  let
    initialState = SimpleState False
    commands = [cRegisterFirst env, cRegisterFirstForbidden env]
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands
  liftIO reset
  executeSequential initialState actions
