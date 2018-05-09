{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API.Player where

import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Log           (MonadLog)
import qualified Control.Monad.Log           as Log
import           Control.Monad.Log.Label     (Label (Label), withLabel)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Crypto.Scrypt               (EncryptedPass (..), Pass (..),
                                              verifyPass')
import           Data.ByteString             as ByteString
import qualified Data.ByteString.Lazy.Char8  as BSL8
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Beam               (unAuto)
import           Servant                     ((:<|>) ((:<|>)), (:>), Get,
                                              Header, Headers, JSON,
                                              NoContent (NoContent), PlainText,
                                              PostNoContent, ReqBody,
                                              ServantErr, ServerT, err401,
                                              err403, err500, errBody)
import           Servant.Auth.Server         (Auth, AuthResult (Authenticated),
                                              CookieSettings, JWTSettings,
                                              SetCookie, acceptLogin, makeJWT)

import           Leaderboard.Env             (HasDbConnPool, withConn)
import           Leaderboard.Queries         (insertPlayer, selectPlayerByEmail,
                                              selectPlayerById,
                                              selectPlayerCount)
import           Leaderboard.Schema          (Player, PlayerT (..))
import           Leaderboard.Types           (Login (..),
                                              PlayerCount (PlayerCount),
                                              PlayerSession (..),
                                              RegisterPlayer (..))

type PlayerAPI auths =
       Auth auths PlayerSession :> "register" :> ReqBody '[JSON] RegisterPlayer :> PostNoContent '[JSON] NoContent
  :<|> "register-first" :> ReqBody '[JSON] RegisterPlayer :> PostNoContent '[JSON] (AuthHeaders NoContent)
  -- TODO ajmccluskey: should probably make this return JSON out of consistency
  :<|> "login" :> ReqBody '[JSON] Login :> PostNoContent '[PlainText] (AuthHeaders ByteString)
  :<|> "player-count" :> Get '[JSON] PlayerCount

type AuthHeaders = Headers '[Header "Set-Cookie" SetCookie , Header "Set-Cookie" SetCookie]

playerServer
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> ServerT (PlayerAPI auths) m
playerServer cs jwts =
       register
  :<|> registerFirst cs jwts
  :<|> login cs jwts
  :<|> playerCount

register
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult PlayerSession
  -> RegisterPlayer
  -> m NoContent
register arp rp =
  withLabel (Label "/register") $
  case arp of
    Authenticated PlayerSession{..} -> do
      ePlayer <- withConn $ \conn -> liftIO $ selectPlayerById conn _psId
      case ePlayer of
        Left e -> do
          Log.info $ "Failed authentication: " <> T.pack (show e)
          throwError err401
        Right Player{..} ->
          if _playerIsAdmin
            then withConn $ \conn -> liftIO (NoContent <$ insertPlayer conn rp)
            else throwError $ err401 {errBody = "Must be an admin to register a new player"}
    ar ->  do
      Log.info $ "Failed authentication: " <> T.pack (show ar)
      throwError $ err401 {errBody = BSL8.pack (show ar)}

registerFirst
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> RegisterPlayer
  -> m (AuthHeaders NoContent)
registerFirst cs jwts rp = do
  numPlayers' <- withConn $ liftIO . selectPlayerCount
  numPlayers <- either (const $ throwError err500) pure numPlayers'
  if numPlayers < 1
    then addFirstPlayer cs jwts rp
    else throwError $ err403 { errBody = "First user already added." }

login
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> Login
  -> m (AuthHeaders ByteString)
login cs jwts Login{..} =
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
        then acceptLogin' cs jwts p
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

acceptLogin'
  :: ( MonadError ServantErr m
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> Player
  -> m (AuthHeaders ByteString)
acceptLogin' cs jwts p@Player{..} = do
  let
    throwNoId = do
      Log.error ("Player with email '" <> _playerEmail <> "' missing id")
      throwError err500
    throwTokenError e = do
      Log.error ("Error creating token for player '" <> T.pack (show p) <> ":")
      Log.error ("    " <> T.pack (show e))
      throwError err500
  pId <- maybe throwNoId pure . unAuto $ _playerId
  eToken <- liftIO $ makeJWT (PlayerSession pId) jwts Nothing
  token <- either throwTokenError (pure . BSL8.toStrict) eToken
  mApplyCookies <- liftIO . acceptLogin cs jwts . PlayerSession $ pId
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> pure $ applyCookies token

addFirstPlayer
  :: ( MonadBaseControl IO m
     , MonadError ServantErr m
     , MonadReader r m
     , HasDbConnPool r
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> RegisterPlayer
  -> m (AuthHeaders NoContent)
addFirstPlayer cs jwts rp = do
  let
    playerThrow = throwError $ err500 { errBody = "User registration failed" }
  -- Force admin flag to true for first registration
  mp <- withConn $ \conn -> liftIO . insertPlayer conn $ rp {_lbrIsAdmin = Just True}
  maybe playerThrow (fmap (NoContent <$) . acceptLogin' cs jwts) mp

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
