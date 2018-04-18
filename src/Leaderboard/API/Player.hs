{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API.Player where

import           Control.Lens
import           Control.Monad                            (void)
import           Control.Monad.Except                     (MonadError,
                                                           throwError)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Monad.Log                        as Log
import           Control.Monad.Reader
import           Control.Monad.Trans.Control              (MonadBaseControl)
import           Data.Aeson                               (FromJSON, parseJSON,
                                                           withObject, (.:))
import           Data.Semigroup                           ((<>))
import           Data.Text                                (Text)
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Network.HTTP.Client.TLS
import           Servant                                  (Server, (:<|>) ((:<|>)),
                                                           (:>), Header,
                                                           Headers, JSON,
                                                           NoContent (NoContent),
                                                           Post, PostNoContent,
                                                           ReqBody, ServerT,
                                                           err401, err403,
                                                           errBody)
import           Servant.Auth.Server                      (Auth, AuthResult (Authenticated),
                                                           CookieSettings,
                                                           JWTSettings,
                                                           SetCookie,
                                                           acceptLogin)
import           URI.ByteString.QQ

import           Leaderboard.Env                          (HasDbConnPool,
                                                           withConn)
import           Leaderboard.Queries                      (addPlayer,
                                                           selectPlayerCount)
import           Leaderboard.Schema                       (Player, PlayerT (..))
import           Leaderboard.Types                        (LeaderboardError (ServantError),
                                                           RegisterPlayer (..))

type PlayerAPI auths =
       Auth auths Player :> "register" :> ReqBody '[JSON] RegisterPlayer :> PostNoContent '[JSON] NoContent
  :<|> "register-first" :> ReqBody '[JSON] RegisterPlayer :> PostNoContent '[JSON] (AuthHeaders NoContent)

type AuthHeaders = Headers '[Header "Set-Cookie" SetCookie , Header "Set-Cookie" SetCookie]

playerServer
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadIO m
     , MonadReader r m
     , MonadError LeaderboardError m
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
     , MonadIO m
     , MonadReader r m
     , MonadError LeaderboardError m
     )
  => AuthResult Player
  -> RegisterPlayer
  -> m NoContent
register (Authenticated Player{..}) rp =
  case unAuto _playerIsAdmin of
    Just True -> liftIO $ NoContent <$ addPlayer rp
    _         -> throwError . ServantError $ err403 {errBody = "Must be an admin to register a new player"}

registerFirst
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadIO m
     , MonadReader r m
     , MonadError LeaderboardError m
     )
  => CookieSettings
  -> JWTSettings
  -> RegisterPlayer
  -> m (AuthHeaders NoContent)
registerFirst cs jwts rp =
  withConn $ \c -> do
    numPlayers <- liftIO $ selectPlayerCount c
    if numPlayers < 1
      then addFirstPlayer cs jwts rp
      else throwError . ServantError $ err403 { errBody = "First user already added." }

addFirstPlayer
  :: ( MonadBaseControl IO m
     , MonadIO m
     , MonadError LeaderboardError m
     )
  => CookieSettings
  -> JWTSettings
  -> RegisterPlayer
  -> m (AuthHeaders NoContent)
addFirstPlayer cs jwts rp = do
  -- Force admin flag to true for first registration
  p <- liftIO . addPlayer $ rp {_lbrIsAdmin = Just True}
  mApplyCookies <- liftIO $ acceptLogin cs jwts p
  case mApplyCookies of
    Nothing           -> throwError . ServantError $ err401
    Just applyCookies -> pure $ applyCookies NoContent
