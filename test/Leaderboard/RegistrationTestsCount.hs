-- This is a simplified version of what `RegistrationTests` does.
-- Used for examples in a talk.

{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Leaderboard.RegistrationTestsCount
  ( registrationTestsCount
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bool                 (bool)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Data.Semigroup            ((<>))
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import           Network.HTTP.Types.Status (forbidden403)
import           Servant.Client            (ClientEnv, ServantError (..))

import           Hedgehog                  (Callback (..), Command (Command),
                                            Eq1, GenT, HTraversable (htraverse),
                                            MonadGen, MonadTest, PropertyT,
                                            Show1, Var (Var), annotateShow,
                                            assert, concrete, evalEither,
                                            executeSequential, failure, forAll,
                                            property, success, test, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)

import           Leaderboard.Schema        (PlayerT (..))
import qualified Leaderboard.Schema        as LS
import           Leaderboard.SharedState   (PlayerMap, PlayerWithRsp (..),
                                            checkCommands, clientToken,
                                            emptyState, failureClient,
                                            genPlayerWithRsp,
                                            successClient)
import           Leaderboard.TestClient    (getPlayerCount, me, register,
                                            registerFirst)
import           Leaderboard.Types         (PlayerCount (..),
                                            RegisterPlayer (..),
                                            ResponsePlayer (..))

data LeaderboardState (v :: * -> *) =
  LeaderboardState
  { _players :: M.Map Text (PlayerWithRsp v)
  , _admins  :: S.Set Text
  }
deriving instance Show1 v => Show (LeaderboardState v)
deriving instance Eq1 v => Eq (LeaderboardState v)

registrationTestsCount
  :: IO ()
  -> ClientEnv
  -> TestTree
registrationTestsCount truncateTables env =
    testGroup "registration-count" [
      propRegister env truncateTables
    ]

genRegPlayerRandomAdmin
  :: MonadGen n
  => PlayerMap v
  -> n RegisterPlayer
genRegPlayerRandomAdmin ps =
  let
    genNonEmptyUnicode = Gen.text (Range.linear 1 20) Gen.alphaNum
  in
    LeaderboardRegistration
      <$> Gen.filter (`S.notMember` M.keysSet ps) genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> genNonEmptyUnicode
      <*> Gen.maybe Gen.bool

genAdminWithRsp
  :: MonadGen n
  => LeaderboardState v
  -> Maybe (n (PlayerWithRsp v))
genAdminWithRsp (LeaderboardState ps as) =
  if null as
  then Nothing
  else pure $ (M.!) ps <$> (Gen.element . S.toList $ as)

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

--------------------------------------------------------------------------------
-- PLAYER COUNT
--------------------------------------------------------------------------------

data GetPlayerCount (v :: * -> *) =
    GetPlayerCount
  deriving (Eq, Show)
instance HTraversable GetPlayerCount where
  htraverse _ _ = pure GetPlayerCount

cGetPlayerCount
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cGetPlayerCount env =
  let
    gen _s = Just . pure $ GetPlayerCount
    exe _i = evalEither =<< successClient env (unPlayerCount <$> getPlayerCount)
  in
    Command gen exe [
      Ensure $ \(LeaderboardState ps as) _sNew _i c -> do
        annotateShow ps
        annotateShow as
        length ps === fromIntegral c
        assert $ length ps >= length as
    ]

newtype Me (v :: * -> *) =
  Me (PlayerWithRsp v)
  deriving (Eq, Show)
instance HTraversable Me where
  htraverse f (Me pwr) = Me <$> htraverse f pwr

cMe
  :: ( MonadGen n
     , MonadTest m
     , MonadIO m
     )
  => ClientEnv
  -> Command n m LeaderboardState
cMe env =
  let
    gen (LeaderboardState ps _as) = bool (fmap Me <$> genPlayerWithRsp ps) Nothing $ null ps
    exe (Me pwr) = evalEither =<< successClient env (me (clientToken pwr))
  in
    Command gen exe
    [ Require $ \(LeaderboardState ps _as) (Me PlayerWithRsp{..}) -> M.member _pwrEmail ps
    , Require $ \(LeaderboardState _ps as) _in -> not (null as)
    , Ensure $ \(LeaderboardState ps _as) _sNew _i p@Player{..} -> do
        let
          pwr@PlayerWithRsp{..} = ps M.! _playerEmail
          -- If there's only one user it should be an admin regardless of what we input
          pwrAdmin = fromMaybe False _pwrIsAdmin
        annotateShow $ length ps
        annotateShow pwr
        annotateShow p
        (_rspId . concrete $ _pwrRsp) === LS.PlayerId _playerId
        _pwrUsername === _playerUsername
        _pwrEmail === _playerEmail
        pwrAdmin === _playerIsAdmin
    ]

--------------------------------------------------------------------------------
-- REGISTER FIRST
--------------------------------------------------------------------------------

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

cRegisterFirst
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) LeaderboardState
cRegisterFirst env =
  let
    gen (LeaderboardState ps _as) =
      if null ps
      then Just $ RegFirst <$> genRegPlayerRandomAdmin ps
      else Nothing
    execute (RegFirst rp) =
       -- Force admin flag to true so our local state always aligns with DB
       evalEither =<< successClient env (registerFirst $ rp {_lbrIsAdmin = Just True})
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as) _input -> null ps
    , Update $ \(LeaderboardState _ps _as) (RegFirst lbr@LeaderboardRegistration{..}) rsp ->
        let lbr' = lbr {_lbrIsAdmin = Just True}
         in LeaderboardState (M.singleton _lbrEmail (mkPlayerWithRsp lbr' rsp)) (S.singleton _lbrEmail)
    , Ensure $ \_sOld (LeaderboardState psNew _as) (RegFirst _rp) _t -> length psNew === 1
    ]

cRegisterFirstForbidden
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) LeaderboardState
cRegisterFirstForbidden env =
  let
    gen (LeaderboardState ps _as) =
      bool (Just $ RegFirstForbidden <$> genRegPlayerRandomAdmin ps) Nothing $ null ps
    execute (RegFirstForbidden rp) =
      evalEither =<< failureClient env (registerFirst rp)
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as) _input -> not (null ps)
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
  Register RegisterPlayer (PlayerWithRsp v)
  deriving (Eq, Show)
instance HTraversable Register where
  htraverse f (Register rp PlayerWithRsp{..}) =
    let mkFP (Var rsp) = fmap (\_pwrRsp -> PlayerWithRsp{..}) $ Var <$> f rsp
     in Register rp <$> mkFP _pwrRsp

cRegister
  :: MonadGen n
  => ClientEnv
  -> Command n (PropertyT IO) LeaderboardState
cRegister env =
  let
    gen rs@(LeaderboardState ps as) =
      if null as
      then Nothing
      else (Register <$> genRegPlayerRandomAdmin ps <*>) <$> genAdminWithRsp rs
    execute (Register rp p) =
      evalEither =<< successClient env (register (clientToken p) rp)
  in
    Command gen execute [
      Require $ \(LeaderboardState ps _as) (Register rp _p) ->
        M.notMember (_lbrEmail rp) ps
    , Require $ \(LeaderboardState _ps as) (Register _rp p) ->
        S.member (_pwrEmail p) as
    , Update $ \(LeaderboardState ps as) (Register rp@LeaderboardRegistration{..} _rqToken) rsp ->
        let
          newPlayers = M.insert _lbrEmail (mkPlayerWithRsp rp rsp) ps
          newAdmins =
            case _lbrIsAdmin of
              Just True -> S.insert _lbrEmail as
              _         -> as
        in
          LeaderboardState newPlayers newAdmins
    , Ensure $ \(LeaderboardState psOld asOld)
                (LeaderboardState psNew asNew)
                (Register LeaderboardRegistration{..} _t)
                _output -> do
        assert $ M.member _lbrEmail psNew
        assert $ M.notMember _lbrEmail psOld
        length psNew === length psOld + 1
        case _lbrIsAdmin of
          Just True -> do
            assert $ S.member _lbrEmail asNew
            assert $ S.notMember _lbrEmail asOld
            length asNew === length asOld + 1
          _ -> success
    ]

propRegister
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegister env reset =
  let
    initialState = LeaderboardState M.empty S.empty
  in
    checkCommands "register-count" reset initialState $
      ($ env) <$> [cRegister, cRegisterFirst, cRegisterFirstForbidden, cGetPlayerCount]

