{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.RegistrationTestsSimple
  ( registrationTestsSimple
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.ByteString           as BS
import           Database.Beam             (Auto (Auto))
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            HTraversable (htraverse), MonadGen,
                                            MonadTest, assert, evalEither,
                                            executeSequential, failure, forAll,
                                            property, test, (===), Symbolic, Concrete)
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import qualified Leaderboard.Schema        as LS
import           Leaderboard.SharedState   (failureClient, successClient)
import           Leaderboard.TestClient    (registerFirst)
import           Leaderboard.Types         (RegisterPlayer (..),
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

cRegisterFirstGen
  :: MonadGen n
  => SimpleState Symbolic
  -> Maybe (n (RegFirst Symbolic))
cRegisterFirstGen (SimpleState registeredFirst) =
  if registeredFirst
  then Nothing
  else Just (RegFirst <$> genRegPlayerRandomAdmin)

cRegisterFirstExe
  :: ( MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> RegFirst Concrete
  -> m ResponsePlayer
cRegisterFirstExe env (RegFirst rp) =
  evalEither =<< successClient env (registerFirst rp)

cRegisterFirstCallbacks
  :: [Callback RegFirst ResponsePlayer SimpleState]
cRegisterFirstCallbacks =
  [ Require $ \(SimpleState registeredFirst) _i ->
      not registeredFirst
  , Update $ \_sOld _i _o -> SimpleState True
  , Ensure $ \_sOld _sNew _i rsp ->
      case rsp of
        (ResponsePlayer (LS.PlayerId (Auto mId))
                        (Token token)) -> do
          assert $ not (BS.null token)
          assert $ maybe False (>= 0) mId
  ]

cRegisterFirst
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m SimpleState
cRegisterFirst env =
  Command cRegisterFirstGen
          (cRegisterFirstExe env)
          cRegisterFirstCallbacks

cRegisterFirstForbiddenGen (SimpleState registeredFirst) =
  if registeredFirst
  then Just (RegFirstForbidden <$> genRegPlayerRandomAdmin)
  else Nothing

cRegisterFirstForbiddenExe (RegFirstForbidden rp) =
  evalEither =<< failureClient env (registerFirst rp)

cRegisterFirstForbiddenCallbacks = [
    Require $ \(SimpleState registeredFirst) _input ->
      registeredFirst
  , Ensure $ \_sOld _sNew _input se ->
      case se of
        FailureResponse{..} -> responseStatus === forbidden403
        _                   -> failure
  ]

cRegisterFirstForbidden
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m SimpleState
cRegisterFirstForbidden env =
  Command cRegisterFirstForbiddenGen
          (cRegisterFirstForbiddenExe env)
          cRegisterFirstForbiddenCmds

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

  -- Actions run after all generators have run, but before execution, get run before
  -- every execution -- i.e. before each shrink is executed. One way to signal this
  -- and enforce it in the types is to put these actions in `TestT`. `TestT` does not
  -- allow generators to run (that requires `PropertyT`), and can be transformed to a
  -- `PropertyT` using `test`.
  test $ do
    liftIO reset
    executeSequential initialState actions
