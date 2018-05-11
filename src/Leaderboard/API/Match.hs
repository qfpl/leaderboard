{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API.Match where

import           Control.Monad.Except        (ExceptT (ExceptT), MonadError,
                                              throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Log           (MonadLog)
import qualified Control.Monad.Log           as Log
import           Control.Monad.Log.Label     (Label (Label), withLabel)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Crypto.Scrypt               (EncryptedPass (..), Pass (..),
                                              verifyPass')
import qualified Data.ByteString.Lazy.Char8  as BSL8
import           Data.Maybe                  (fromMaybe)
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Beam               (unAuto)
import           Database.PostgreSQL.Simple  (Connection)
import           Servant                     ((:<|>) ((:<|>)), (:>), Capture,
                                              DeleteNoContent, Get, Header,
                                              Headers, JSON, NoContent, Post,
                                              PostNoContent, PutNoContent,
                                              ReqBody, ServantErr, ServerT,
                                              err401, err403, err500, errBody)
import           Servant.Auth.Server         (Auth, AuthResult (Authenticated),
                                              CookieSettings, JWTSettings,
                                              SetCookie, acceptLogin, makeJWT)

import           Leaderboard.Env             (HasDbConnPool, asPlayer, withConn)
import           Leaderboard.Queries         (selectMatches, insertMatch, insertPlayer,
                                              selectPlayerByEmail,
                                              selectPlayerById,
                                              selectPlayerCount)
import           Leaderboard.Schema          (Match, MatchId, Player, PlayerId,
                                              PlayerT (..))
import           Leaderboard.Types           (Login (..),
                                              PlayerCount (PlayerCount),
                                              PlayerSession (..),
                                              RegisterPlayer (..), RqMatch (..),
                                              Token (..), tryJustPgError)

type MatchAPI auths =
  Auth auths PlayerSession :> "matches" :>
  (      "add" :> ReqBody '[JSON] RqMatch :> PostNoContent '[JSON] Int
    :<|> "list" :> Get '[JSON] [Match]
    :<|> Capture "id" MatchId :> DeleteNoContent '[JSON] NoContent
    :<|> Capture "id" MatchId :> ReqBody '[JSON] RqMatch :> PutNoContent '[JSON] NoContent
  )

matchServer
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => ServerT (MatchAPI auths) m
matchServer arp =
        addMatch arp
  :<|> listMatches arp
  :<|> deleteMatch arp
  :<|> editMatch arp

addMatch
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult PlayerSession
  -> RqMatch
  -> m Int
addMatch arp match =
  withAuthConnAndLog arp "/matches/add" $ \conn -> do
  let
    bad e = do
      Log.error $ "Error inserting player: " <> (pack . show $ e)
      throwError $ err500 {errBody = "Insert of match failed"}
  eId <- liftIO $ insertMatch conn match
  either bad pure eId

listMatches
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult PlayerSession
  -> m [Match]
listMatches arp =
  withAuthConnAndLog arp "/matches/list" $ \conn -> do
  let
    bad e = do
      Log.error $ "Error retrieving matches: " <> (pack . show $ e)
      throwError $ err500 { errBody = "Error retrieving matches" }
  el <- liftIO . tryJustPgError $ selectMatches conn
  either bad pure el

deleteMatch
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult PlayerSession
  -> MatchId
  -> m NoContent
deleteMatch =
  undefined

editMatch
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult PlayerSession
  -> MatchId
  -> RqMatch
  -> m NoContent
editMatch =
  undefined

withAuthConnAndLog
  :: ( MonadError ServantErr m
     , MonadLog Label m
     , HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     )
  => AuthResult PlayerSession
  -> Text
  -> (Connection -> m a)
  -> m a
withAuthConnAndLog arp label f =
  asPlayer arp $ \_pId ->
  withLabel (Label label) $
  withConn $ \conn -> f conn
