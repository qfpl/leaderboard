{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where



import           Control.Concurrent            (forkIO, newEmptyMVar, takeMVar,
                                                throwTo)
import           Control.Exception             (Exception, bracket, throw)
import           Control.Lens                  ((&), (.~))
import qualified Control.Monad.Log             as Log
import           Control.Monad.Log.Label       (Label)
import           Database.Postgres.Temp        (DB (..), SocketClass (Unix),
                                                StartError, startWithHandles,
                                                stop)
import           Network.Connection            (TLSSettings (..))
import           Network.HTTP.Client.TLS       (mkManagerSettings,
                                                newTlsManagerWith)
import           Servant                       ((:~>), Application,
                                                Context ((:.), EmptyContext),
                                                Handler, enter,
                                                serveWithContext)
import           Servant.Auth.Server           (defaultCookieSettings,
                                                defaultJWTSettings)
import           Servant.Client                (BaseUrl (BaseUrl),
                                                ClientEnv (..), Scheme (Https))
import           System.IO                     (IOMode (WriteMode), openFile)

import           Test.Tasty                    (TestTree, defaultMain,
                                                testGroup)

import           Leaderboard.Env               (Env, _envJWK)
import           Leaderboard.JsonTests         (jsonTests)
import           Leaderboard.Main              (doTheLeaderboard)
import           Leaderboard.MatchTests        (matchTests)
import           Leaderboard.RegistrationTests (registrationTests)
import           Leaderboard.Server            (LHandlerT, toHandler)
import           Leaderboard.TestAPI       (testAPI, testServer)
import           Leaderboard.TestHelpers       (truncateTables)
import           Leaderboard.Types             (ApplicationOptions (..),
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
  doSomeTests allTheTests

doSomeTests ::
  (IO () -> ClientEnv -> TestTree)
  -> IO ()
doSomeTests ts =
  withDb $ \DB{..} -> do
  let
    ao = ApplicationOptions connectionInfo 7645 RunApp (Just Log.levelWarning)
    tt = truncateTables connectionInfo
  withLeaderboard ao (defaultMain . ts tt)

allTheTests
  :: IO ()
  -> ClientEnv
  -> TestTree
allTheTests tt env =
  testGroup "leaderboard"
  [ registrationTests tt env
  , matchTests tt env
  , jsonTests
  ]

withDb
  :: (DB -> IO a)
  -> IO a
withDb f =
  bracket
    (startAndLogDbInDirectory [])
    (splode stop)
    (splode f)
  where
    splode g = \case
      Left e   -> throw . PgTempStartError $ e
      Right db -> g db

startAndLogDbInDirectory ::
  [(String, String)]
  -> IO (Either StartError DB)
startAndLogDbInDirectory options = do
  stdOutFile <- openFile "./tmp-postgres-output.txt" WriteMode
  stdErrFile <- openFile "./tmp-postgres-error.txt" WriteMode

  startWithHandles Unix options stdOutFile stdErrFile

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
        doTheLeaderboard testApp Nothing aoMigrate
        doTheLeaderboard testApp (Just ready) ao
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

testApp ::
  Env
  -> Log.Logger Label
  -> Application
testApp env logger =
  let
    jwtCfg = defaultJWTSettings (_envJWK env)
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    toHandler' :: LHandlerT Env Handler :~> Handler
    toHandler' = toHandler env logger
    server = testServer jwtCfg
  in
    serveWithContext testAPI cfg $ enter toHandler' server
