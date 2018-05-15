{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API.Player where

import           Control.Monad.Except                   (MonadError, throwError)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Log                      (MonadLog)
import qualified Control.Monad.Log                      as Log
import           Control.Monad.Log.Label                (Label (Label),
                                                         withLabel)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control            (MonadBaseControl)
import           Crypto.Scrypt                          (EncryptedPass (..),
                                                         Pass (..), verifyPass')
import           Data.Bool                              (bool)
import qualified Data.ByteString.Lazy.Char8             as BSL8
import           Data.Proxy                             (Proxy (Proxy))
import           Data.Semigroup                         ((<>))
import qualified Data.Text                              as T
import           Data.Text.Encoding                     (encodeUtf8)
import           Database.Beam                          (unAuto)
import           Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import           Servant                                ((:<|>) ((:<|>)), (:>),
                                                         Get, JSON, Post,
                                                         ReqBody, ServantErr,
                                                         ServerT, err401,
                                                         err403, err500,
                                                         errBody)
import           Servant.Auth.Server                    (Auth, AuthResult,
                                                         JWTSettings, makeJWT)

import           Leaderboard.Env                        (HasDbConnPool,
                                                         asPlayer,
                                                         withAuthConnAndLog,
                                                         withConn)
import           Leaderboard.Queries                    (insertPlayer,
                                                         selectPlayerByEmail,
                                                         selectPlayerById,
                                                         selectPlayerCount)
import           Leaderboard.Schema                     (Player, PlayerT (..))
import qualified Leaderboard.Schema                     as LS
import           Leaderboard.Types                      (LeaderboardError (PlayerExists),
                                                         Login (..),
                                                         PlayerCount (PlayerCount),
                                                         PlayerSession (..),
                                                         RegisterPlayer (..),
                                                         RspPlayer (..),
                                                         Token (..))

type PlayerAPI auths =
  "player" :> (
       Auth auths PlayerSession :> "register" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] RspPlayer
  :<|> "register-first" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] RspPlayer
  :<|> Auth auths PlayerSession :> "me" :> Get '[JSON] Player
  :<|> "authenticate" :> ReqBody '[JSON] Login :> Post '[JSON] Token
  :<|> "player-count" :> Get '[JSON] PlayerCount
  )

playerAPI :: Proxy (PlayerAPI auths)
playerAPI = Proxy

playerServer
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => JWTSettings
  -> ServerT (PlayerAPI auths) m
playerServer jwts =
       register jwts
  :<|> registerFirst jwts
  :<|> me
  :<|> authenticate jwts
  :<|> playerCount

register
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => JWTSettings
  -> AuthResult PlayerSession
  -> RegisterPlayer
  -> m RspPlayer
register jwts arp rp =
  withLabel (Label "/register") $
  asPlayer arp $ \psId -> do
    ePlayer <- withConn $ \conn -> liftIO $ selectPlayerById conn psId
    case ePlayer of
      Left e -> do
        Log.info $ "Failed authentication: " <> T.pack (show e)
        throwError $ err401 { errBody = "Please try reauthenticating" }
      Right Player{..} ->
        if _playerIsAdmin
          then (pure . RspPlayer (LS.PlayerId _playerId) <=< makeToken jwts <=< playerId <=< insertPlayer') rp
          else throwError $ err401 {errBody = "Must be an admin to register a new player"}

registerFirst
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => JWTSettings
  -> RegisterPlayer
  -> m RspPlayer
registerFirst jwts rp =
  withLabel (Label "/register-first") $ do
  -- Possible race condition between checking count and inserting -- transaction it
  ep <- withConn $ \conn -> liftIO . withTransactionSerializable conn $ do
    let
      rp' = rp {_lbrIsAdmin = Just True}
      insert = bool (pure . Left $ PlayerExists) (insertPlayer conn rp') . (< 1)
    numPlayers <- selectPlayerCount conn
    either (pure . Left) insert numPlayers
  case ep of
    Left PlayerExists -> do
      Log.info "registerFirst called but player(s) already registered"
      throwError $ err403 { errBody = "First user already added." }
    Left e -> do
      Log.error $ "While inserting player (email = '" <> _lbrEmail rp <> "'): " <> T.pack (show e)
      throwError err500
    Right p@Player{..} -> do
      pId <- playerId p
      Log.debug $ "Inserted new player: " <> T.pack (show p)
      token <- makeToken jwts pId
      pure $ RspPlayer (LS.PlayerId _playerId) token

me
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult PlayerSession
  -> m Player
me arp =
  withAuthConnAndLog arp "/players/me" $ \pId conn -> do
    ePlayer <- liftIO $ selectPlayerById conn pId
    case ePlayer of
      Left e -> do
        Log.info $ "Failed authentication: " <> T.pack (show e)
        throwError $ err401 { errBody = "Please try reauthenticating" }
      Right p -> pure p


authenticate
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => JWTSettings
  -> Login
  -> m Token
authenticate jwts Login{..} =
  withLabel (Label "/login") $ do
  let
    loginPass = Pass . encodeUtf8 $ _loginPassword
    throwLoginFail e = do
      Log.info $ "Failed login: " <> T.pack (show e)
      throwError (err401 { errBody = "Login failed" })
  ePlayer <- withConn $ \conn -> liftIO $ selectPlayerByEmail conn _loginEmail
  case ePlayer of
    Left e           -> throwLoginFail e
    Right p@Player{..} ->
      if verifyPass' loginPass (EncryptedPass _playerPassword)
        then playerId p >>= makeToken jwts
        else throwLoginFail ("Bad password" :: T.Text)

playerCount
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadError ServantErr m
     , MonadReader r m
     , MonadLog Label m
     )
  => m PlayerCount
playerCount =
  withLabel (Label "/player-count") $ PlayerCount <$> getPlayerCount

insertPlayer'
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => RegisterPlayer
  -> m Player
insertPlayer' rp =
  withLabel (Label "insertPlayer") $ do
  Log.debug $ "Inserting player: " <> (T.pack . show $ rp)
  let
    throwNoPlayer e = do
      Log.error . T.pack . show $ e
      throwError $ err500 {errBody = "Error registering player"}
  ep <- withConn $ \conn -> liftIO $ insertPlayer conn rp
  either throwNoPlayer pure ep

getPlayerCount
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadError ServantErr m
     , MonadReader r m
     , MonadLog Label m
     )
  => m Integer
getPlayerCount = do
  let
    throwNoPlayerCount e = do
      Log.error $ "Error retrieving player count: " <> (T.pack . show $ e)
      throwError err500 { errBody = "Error retrieving player count" }
  en <- withConn $ liftIO . selectPlayerCount
  either throwNoPlayerCount pure en

playerId
  :: ( MonadError ServantErr m
     , MonadLog Label m
     )
  => Player
  -> m Int
playerId Player{..} =
  let
    throwNoId = do
      Log.error ("Player with email '" <> _playerEmail <> "' missing id")
      throwError err500
  in
    maybe throwNoId pure . unAuto $ _playerId

makeToken
  :: ( MonadError ServantErr m
     , MonadLog Label m
     )
  => JWTSettings
  -> Int
  -> m Token
makeToken jwts pId = do
  let
    throwTokenError e = do
      Log.error ("Error creating token for player with id '" <> T.pack (show pId) <> ":")
      Log.error ("    " <> T.pack (show e))
      throwError err500
  eToken <- liftIO $ makeJWT (PlayerSession pId) jwts Nothing
  either throwTokenError (pure . Token . BSL8.toStrict) eToken
