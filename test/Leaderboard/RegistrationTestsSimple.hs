{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Leaderboard.RegistrationTestsSimple
  ( registrationTestsSimple
  ) where

import           Control.Monad.IO.Class    (liftIO, MonadIO)
import qualified Data.ByteString           as BS
import           Data.Semigroup            ((<>))
import           Database.Beam             (Auto (Auto))
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            HTraversable (htraverse), MonadGen,
                                            PropertyT, Var, assert,
                                            executeSequential, failure, forAll,
                                            property, test, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import qualified Leaderboard.Schema        as LS
import           Leaderboard.SharedState   (PlayerWithRsp (..), failureClient,
                                            successClient)
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

cRegisterFirst
  :: ( MonadGen n
     , MonadIO m
     )
  => ClientEnv
  -> Command n m SimpleState
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
    , Ensure $ \_sOld _sNew _input out ->
        case out of
          (ResponsePlayer (LS.PlayerId (Auto mId)) _token) ->
            assert $ maybe False (>= 0) mId
    , Ensure $ \_sOld _sNew _input out ->
        case out of
          (ResponsePlayer _pId (Token t)) ->
            assert $ not (BS.null t)
    ]

cRegisterFirstForbidden
  :: ( MonadGen n
     , MonadIO m
     )
  => ClientEnv
  -> Command n m SimpleState
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

  -- Actions run after all generators have run, but before execution, get run before
  -- every execution -- i.e. before each shrink is executed. One way to signal this
  -- and enforce it in the types is to put these actions in `TestT`. `TestT` does not
  -- allow generators to run (that requires `PropertyT`), and can be transformed to a
  -- `PropertyT` using `test`.
  test $ do
    liftIO reset
    executeSequential initialState actions
