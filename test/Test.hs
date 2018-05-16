{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent                  (forkIO, newEmptyMVar,
                                                      takeMVar, throwTo)
import           Control.Exception                   (Exception, bracket, throw)
import           Control.Lens                        ((&), (.~))
import qualified Control.Monad.Log                   as Log
import           Database.Postgres.Temp              (DB (..), StartError,
                                                      startAndLogToTmp, stop)
import           Network.Connection                  (TLSSettings (..))
import           Network.HTTP.Client.TLS             (mkManagerSettings,
                                                      newTlsManagerWith)
import           Servant.Client                      (BaseUrl (BaseUrl),
                                                      ClientEnv (..),
                                                      Scheme (Https))
import           System.Directory                    (copyFile)
import           System.FilePath                     ((</>))

import           Test.Tasty                          (TestTree, defaultMain,
                                                      testGroup)

import           Leaderboard.Main                    (doTheLeaderboard)
import           Leaderboard.MatchTests              (matchTests)
import           Leaderboard.RegistrationTests       (registrationTests)
import           Leaderboard.RegistrationTestsSimple (registrationTestsSimple)
import           Leaderboard.TestServer              (truncateTables)
import           Leaderboard.Types                   (ApplicationOptions (..),
                                                      Command (..), command)

data Shutdown = Shutdown deriving (Show)
instance Exception Shutdown

data DbInitError =
  PgTempStartError StartError
  | PgUrlError String
  deriving (Show)
instance Exception DbInitError

main :: IO ()
main =
  withDb $ \DB{..} -> do
  let
    ao = ApplicationOptions connectionInfo 7645 RunApp (Just Log.levelWarning)
    tt = truncateTables connectionInfo
  withLeaderboard ao (defaultMain . allTheTests tt)

allTheTests
  :: IO ()
  -> ClientEnv
  -> TestTree
allTheTests tt env =
  testGroup "leaderboard"
  [ registrationTestsSimple tt env
  , registrationTests tt env
  , matchTests tt env
  ]

withDb
  :: (DB -> IO a)
  -> IO a
withDb f =
  bracket
    (startAndLogToTmp [])
    (splode stop')
    (splode f)
  where
    splode g r =
      case r of
        Left e   -> throw . PgTempStartError $ e
        Right db -> g db
    stop' db@DB{..} = do
      copyFile (mainDir </> "output.txt") "tmp-postgres-output.txt"
      copyFile (mainDir </> "error.txt") "tmp-postgres-error.txt"
      stop db

withLeaderboard
  :: ApplicationOptions
  -> (ClientEnv -> IO a)
  -> IO a
withLeaderboard ao@ApplicationOptions{..} =
  let
    aoMigrate = ao & command .~ MigrateDb
    setupLeaderboard = do
      ready <- newEmptyMVar
      thread <- forkIO $ do
        doTheLeaderboard Nothing aoMigrate
        doTheLeaderboard (Just ready) ao
      takeMVar ready
      pure thread
    makeEnv = do
      let
        tlsSettings = TLSSettingsSimple
                      { settingDisableCertificateValidation = True
                      , settingDisableSession = False
                      , settingUseServerName = False
                      }
      tlsManager <- newTlsManagerWith $ mkManagerSettings tlsSettings Nothing
      pure . ClientEnv tlsManager $ BaseUrl Https "localhost" _port ""
  in
    bracket setupLeaderboard (`throwTo` Shutdown) . const . (makeEnv >>=)
