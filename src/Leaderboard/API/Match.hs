{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API.Match where

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
import           Servant                     ((:<|>) ((:<|>)), (:>), Capture,
                                              DeleteNoContent, Get, Header,
                                              Headers, JSON, NoContent, Post,
                                              PostNoContent, PutNoContent,
                                              ReqBody, ServantErr, ServerT,
                                              err401, err403, err500, errBody)
import           Servant.Auth.Server         (Auth, AuthResult (Authenticated),
                                              CookieSettings, JWTSettings,
                                              SetCookie, acceptLogin, makeJWT)

import           Leaderboard.Env             (HasDbConnPool, withConn)
import           Leaderboard.Queries         (insertPlayer, selectPlayerByEmail,
                                              selectPlayerById,
                                              selectPlayerCount)
import           Leaderboard.Schema          (Match, MatchId, Player,
                                              PlayerT (..), RegisterPlayer (..), RqMatch (..), PlayerId)
import           Leaderboard.Types           (Login (..),
                                              PlayerCount (PlayerCount),
                                              PlayerSession (..), Token (..))

type MatchAPI auths =
  Auth auths PlayerSession :> "matches" :>
  (      "add" :> ReqBody '[JSON] RqMatch :> PostNoContent '[JSON] NoContent
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
matchServer _arp =
        addMatch
  :<|> listMatches
  :<|> deleteMatch
  :<|> editMatch

addMatch
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => RqMatch
  -> m NoContent
addMatch match =
  undefined
  -- withLabel (Label "/register") $
  -- case arp of
  --   Authenticated PlayerSession{..} -> do
  --     ePlayer <- withConn $ \conn -> liftIO $ selectPlayerById conn _psId
  --     case ePlayer of
  --       Left e -> do
  --         Log.info $ "Failed authentication: " <> T.pack (show e)
  --         throwError err401
  --       Right Player{..} ->
  --         if _playerIsAdmin
  --           then (makeToken jwts <=< playerId <=< insertPlayer') rp
  --           else throwError $ err401 {errBody = "Must be an admin to register a new player"}
  --   ar ->  do
  --     Log.info $ "Failed authentication: " <> T.pack (show ar)
  --     throwError $ err401 {errBody = BSL8.pack (show ar)}

listMatches
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => m [Match]
listMatches =
  undefined

deleteMatch
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => MatchId
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
  => MatchId
  -> RqMatch
  -> m NoContent
editMatch =
  undefined
