{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Importer
import Data.Map as M
import Data.IORef
import System.Environment
import Util.StreamUtil

-- import CloudLogger.LogServer
import NetworkHub.HubServer
import NetworkHub.HubApps
import Constants
import System.Console.CmdArgs
import System.Log.Logger
import Util.ZipUtil

data Hpayd = Hpayd {
    port :: String
  , level :: String
} deriving (Data, Typeable, Show, Eq)

mode = cmdArgsMode Hpayd {
    port = "80" &= help "server port" &= typ "PORT"
  , level = "INFO" &= help "log level" &= typ "DEBUG|INFO|WARNING|ERROR"
}

_name = "main"

hpayd Hpayd{..} = withSocketsDo $ do
    updateGlobalLogger rootLoggerName $ setLevel $ read level
    appRefs <- newIORef M.empty

--     app1 <- logServerApp
--     modifyIORef appRefs $ M.insert "logServer" app1
    app2 <- hubServerApp [(1, nameService)]
    modifyIORef appRefs $ M.insert "hubServer" app2
--     modifyIORef appRefs $ M.insert "proxyServer" proxyApp

    apps <- readIORef appRefs
    infoM _name $ "start server on port " ++ port
    serve HostIPv4 port $ withSocketStream $ \inn out -> do
        appcode <- readBinary inn
        infoM _name $ "appcode="++appcode
        case M.lookup appcode apps of
            Just app -> app inn out
            Nothing  -> warningM _name $ "unknown appcode:" ++ appcode

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["----source"]  -> unzipFiles "hpayTools" sourceCodes
        _           -> cmdArgsRun mode >>= hpayd

-- test function
test = hpayd Hpayd{ port = "9999", level = "INFO"}