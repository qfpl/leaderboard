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
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text, pack)
import           Database.PostgreSQL.Simple  (Connection)
import           Servant                     ((:<|>) ((:<|>)), (:>), Capture,
                                              DeleteNoContent, Get, JSON,
                                              NoContent, PostNoContent,
                                              PutNoContent, ReqBody, ServantErr,
                                              ServerT, err500, errBody)
import           Servant.Auth.Server         (Auth, AuthResult)

import           Leaderboard.Env             (HasDbConnPool, asPlayer, withConn)
import           Leaderboard.Queries         (insertMatch, selectMatches)
import           Leaderboard.Schema          (Match, MatchId)
import           Leaderboard.Types           (PlayerSession (..), RqMatch (..),
                                              tryJustPgError)

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
