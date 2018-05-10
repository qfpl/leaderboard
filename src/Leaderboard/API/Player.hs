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
import qualified Data.ByteString.Lazy.Char8  as BSL8
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Beam               (unAuto)
import           Servant                     ((:<|>) ((:<|>)), (:>), Get,
                                              Header, Headers, JSON, Post,
                                              ReqBody, ServantErr, ServerT,
                                              err401, err403, err500, errBody)
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
                                              RegisterPlayer (..), Token (..))

type PlayerAPI auths =
       Auth auths PlayerSession :> "register" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] Token
  :<|> "register-first" :> ReqBody '[JSON] RegisterPlayer :> Post '[JSON] (AuthHeaders Token)
  -- TODO ajmccluskey: should probably make this return JSON out of consistency
  :<|> "authenticate" :> ReqBody '[JSON] Login :> Post '[JSON] (AuthHeaders Token)
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
       register jwts
  :<|> registerFirst cs jwts
  :<|> authenticate cs jwts
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
  -> m Token
register jwts arp rp =
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
            then (makeToken jwts <=< playerId <=< insertPlayer') rp
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
  -> m (AuthHeaders Token)
registerFirst cs jwts rp =
  withLabel (Label "/register-first") $ do
  let rp' = rp {_lbrIsAdmin = Just True}
  numPlayers' <- withConn $ liftIO . selectPlayerCount
  numPlayers <- either (const $ throwError err500) pure numPlayers'
  if numPlayers < 1
    then (authenticatePlayerId cs jwts <=< playerId <=< insertPlayer') rp'
    else do
      Log.info $ "registerFirst called but " <> T.pack (show numPlayers) <> " players already registered"
      throwError $ err403 { errBody = "First user already added." }

authenticate
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> Login
  -> m (AuthHeaders Token)
authenticate cs jwts Login{..} =
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
        then playerId p >>= authenticatePlayerId cs jwts
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

authenticatePlayerId
  :: ( MonadError ServantErr m
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> Int
  -> m (AuthHeaders Token)
authenticatePlayerId cs jwts pId = do
  token <- makeToken jwts pId
  addAuthHeaders cs jwts pId token

addAuthHeaders
  :: ( MonadError ServantErr m
     , MonadLog Label m
     )
  => CookieSettings
  -> JWTSettings
  -> Int
  -> response
  -> m (AuthHeaders response)
addAuthHeaders cs jwts pId r = do
  mApplyCookies <- liftIO . acceptLogin cs jwts . PlayerSession $ pId
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> pure $ applyCookies r

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
    throwX msg = do
      Log.error . (msg <>) . T.pack . show $ rp
      throwError $ err500 {errBody = "Error registering player"}
    throwNoPlayer = throwX "Inserting player into database failed -- `Nothing` returned for "
  mp <- withConn $ \conn -> liftIO $ insertPlayer conn rp
  maybe throwNoPlayer pure mp

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
