{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API.Player where

import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Log           (MonadLog)
import qualified Control.Monad.Log           as Log
import           Control.Monad.Log.Label     (Label (Label), withLabel)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Lazy.Char8  as BSL8
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import           Servant                     ((:<|>) ((:<|>)), (:>), Header,
                                              Headers, JSON,
                                              NoContent (NoContent),
                                              PostNoContent, ReqBody,
                                              ServantErr, ServerT, err401,
                                              err403, err500, errBody)
import           Servant.Auth.Server         (Auth, AuthResult (Authenticated),
                                              CookieSettings, JWTSettings,
                                              SetCookie, acceptLogin)

import           Leaderboard.Env             (HasDbConnPool, withConn)
import           Leaderboard.Queries         (addPlayer, selectPlayerCount)
import           Leaderboard.Schema          (Player, PlayerT (..))
import           Leaderboard.Types           (RegisterPlayer (..))

type PlayerAPI auths =
       Auth auths Player :> "register" :> ReqBody '[JSON] RegisterPlayer :> PostNoContent '[JSON] NoContent
  :<|> "register-first" :> ReqBody '[JSON] RegisterPlayer :> PostNoContent '[JSON] (AuthHeaders NoContent)

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

register
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult Player
  -> RegisterPlayer
  -> m NoContent
register arp rp =
  withLabel (Label "/register") $
  case arp of
    Authenticated Player{..} ->
      if _playerIsAdmin
        then withConn $ \conn -> liftIO (NoContent <$ addPlayer conn rp)
        else throwError $ err401 {errBody = "Must be an admin to register a new player"}
    ar ->  do
      Log.info $ "Failed authentication: " <> T.pack (show ar)
      throwError $ err401 {errBody = BSL8.pack (show ar)}

registerFirst
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadIO m
     , MonadReader r m
     , MonadError ServantErr m
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

addFirstPlayer
  :: ( MonadBaseControl IO m
     , MonadIO m
     , MonadError ServantErr m
     , MonadReader r m
     , HasDbConnPool r
     )
  => CookieSettings
  -> JWTSettings
  -> RegisterPlayer
  -> m (AuthHeaders NoContent)
addFirstPlayer cs jwts rp = do
  let
    playerThrow = throwError $ err500 { errBody = "User registration failed" }
  -- Force admin flag to true for first registration
  mp <- withConn $ \conn -> liftIO . addPlayer conn $ rp {_lbrIsAdmin = Just True}
  p <- maybe playerThrow pure mp
  mApplyCookies <- liftIO $ acceptLogin cs jwts p
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> pure $ applyCookies NoContent
