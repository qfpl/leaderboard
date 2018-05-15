{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Leaderboard.API.Match where

import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Log           (MonadLog)
import qualified Control.Monad.Log           as Log
import           Control.Monad.Log.Label     (Label)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Semigroup              ((<>))
import           Data.Text                   (pack)
import           Database.Beam               (Auto (Auto))
import           Database.PostgreSQL.Simple  (Connection)
import           Servant                     ((:<|>) ((:<|>)), (:>), Capture,
                                              DeleteNoContent, Get, JSON,
                                              NoContent, PostNoContent,
                                              PutNoContent, ReqBody, ServantErr,
                                              ServerT, err500, errBody)
import           Servant.Auth.Server         (Auth, AuthResult)

import           Leaderboard.Env             (HasDbConnPool, withAuthConnAndLog)
import           Leaderboard.Queries         (insertMatch, selectMatch,
                                              selectMatches)
import           Leaderboard.Schema          (Match, MatchId)
import qualified Leaderboard.Schema          as LS
import           Leaderboard.Types           (LeaderboardError,
                                              PlayerSession (..), RqMatch (..))

type MatchAPI auths =
  Auth auths PlayerSession :> "matches" :>
  (      "add" :> ReqBody '[JSON] RqMatch :> PostNoContent '[JSON] Int
    :<|> "list" :> Get '[JSON] [Match]
    :<|> "get" :> Capture "id" MatchId :> Get '[JSON] Match
    :<|> Capture "id" MatchId :> DeleteNoContent '[JSON] NoContent
    :<|> Capture "id" MatchId :> ReqBody '[JSON] RqMatch :> PutNoContent '[JSON] NoContent
  )

matchAPI :: Proxy (MatchAPI auths)
matchAPI = Proxy

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
  :<|> getMatch arp
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
  withAuthConnAndLog arp "/matches/add" $ \_pId conn -> do
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
  getMatch'es arp selectMatches

getMatch
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult PlayerSession
  -> MatchId
  -> m Match
getMatch arp mId = do
  mIdOrBust <- matchId mId
  let f conn = selectMatch conn mIdOrBust
  getMatch'es arp f

getMatch'es
  :: ( HasDbConnPool r
     , MonadBaseControl IO m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadLog Label m
     )
  => AuthResult PlayerSession
  -> (Connection -> IO (Either LeaderboardError a))
  -> m a
getMatch'es arp f =
  withAuthConnAndLog arp "/matches/list" $ \_pId conn -> do
  let
    bad e = do
      Log.error $ "Error retrieving match(es): " <> (pack . show $ e)
      throwError $ err500 { errBody = "Error retrieving match(es)" }
  er <- liftIO $ f conn
  either bad pure er

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

matchId
  :: MonadError ServantErr m
  => MatchId
  -> m Int
matchId (LS.MatchId (Auto mId)) =
  maybe (throwError err500) pure mId
