{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf, LambdaCase #-}
module Main where

import Util.StreamUtil

-- import CloudLogger.LogClient
import TcpProxy.TcpProxy
import Constants
import System.Console.CmdArgs
import System.Log.Logger
import System.Environment

import qualified Util.MulticastUtil as Multicast

data HpayCtl =    LogProvider  {hostport :: HostPort
                            ,level :: String
                }
             | LogMonitor   {hostport :: HostPort, level :: String}
             | TcpShare  {hostport :: HostPort,
                             proxy :: String, level :: String }
             | TcpConnect      {hostport :: HostPort,
                             proxy :: String, level :: String }
               deriving (Data, Typeable, Show, Eq)

_hostport x = x &= help "proxy server host:port" &= typ "HOST:PORT"
_level x = x &= help "log level" &= typ "DEBUG|INFO|WARNING|ERROR"

logProvider = LogProvider {
    hostport = _hostport def
  , level = _level "INFO"
} &= help "start a log provider, send log entry to logServer"

logMonitor = LogMonitor {
    hostport = _hostport def
  , level = _level "INFO"
} &= help "start a log monitor, monitor log entry from logServer"

m_tcpShare = TcpShare {
    hostport = _hostport def
  , proxy = "127.0.0.1:80:80" &= help "local host to be proxy, default is 127.0.0.1:80:80" &= typ "LOCALHOST:LOCALPORT:REMOTEPORT"
  , level = _level "INFO"
} &= help "share local tcp server, mainly used for internet access local tcp server"

m_tcpConnect = TcpConnect {
    hostport = _hostport def
  , proxy = "80:80" &= help "local host to be proxy, default is 80:80" &= typ "LOCALPORT:REMOTEPORT"
  , level = _level "INFO"
} &= help "connect to remote shared tcp server, mainly used for connect remote tcp server"

mode = cmdArgsMode $ modes [m_tcpShare, m_tcpConnect]
        &= help "hpayctl tool"
        &= program "hpayctl"
        &= summary ("hpayctl v"++ version)

_name = "hpayctl"

hpayctl cmd = withSocketsDo $ do
    updateGlobalLogger rootLoggerName $ setLevel $ read $ level cmd
    infoM _name $ show cmd
    server <- if
        | null $ hostport cmd -> lookupEnv "HPAYD" >>= \case
            Just v  -> return v
            Nothing -> do
                infoM _name "finding hpayd "
                Multicast.Sender{..} <- Multicast.newSender
                (from, port) <- ask "hpayd"
                return $ from ++ ":" ++ port
        | otherwise -> return $ hostport cmd
    infoM _name $ "connect to " ++ server
    case cmd of
        TcpShare{..} -> tcpSkeleton server proxy
        TcpConnect{..}     -> tcpStub server proxy

main :: IO ()
main = withSource $ cmdArgsRun mode >>= hpayctl

-- test code
t_share     = hpayctl TcpShare{hostport="127.0.0.1:9999", proxy="127.0.0.1:2222:liming/centos", level="INFO"}
t_connect   = hpayctl TcpConnect{hostport="127.0.0.1:9999", proxy="3333:liming/centos", level="INFO"}