{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Util.StreamUtil

-- import CloudLogger.LogClient
import TcpProxy.TcpProxy
import Constants
import System.Console.CmdArgs
import System.Log.Logger

version = "0.6-2015-5"

data HpayCtl =    LogProvider  {hostport :: HostPort
                            ,level :: String
                }
             | LogMonitor   {hostport :: HostPort, level :: String}
             | TcpSkeleton  {hostport :: HostPort,
                             proxy :: String, level :: String }
             | TcpStub      {hostport :: HostPort,
                             proxy :: String, level :: String }
               deriving (Data, Typeable, Show, Eq)

logProvider = LogProvider {
    hostport = def &= help "proxy server host:port" &= typ "HOST:PORT"
  , level = "INFO" &= help "log level" &= typ "DEBUG|INFO|WARNING|ERROR"
} &= help "start a log provider, send log entry to logServer"

logMonitor = LogMonitor {
    hostport = def &= help "log server host:port" &= typ "HOST:PORT"
  , level = "INFO" &= help "log level" &= typ "DEBUG|INFO|WARNING|ERROR"
} &= help "start a log monitor, monitor log entry from logServer"

m_tcpSkeleton = TcpSkeleton {
    hostport = def &= help "tcp proxy server host:port" &= typ "HOST:PORT"
  , proxy = "127.0.0.1:80:80" &= help "local host to be proxy, default is 127.0.0.1:80:80" &= typ "LOCALHOST:LOCALPORT:REMOTEPORT"
  , level = "INFO" &= help "log level" &= typ "DEBUG|INFO|WARNING|ERROR"
} &= help "proxy local tcp server, mainly used for internet access local tcp server"

m_tcpStub = TcpStub {
    hostport = def &= help "tcp proxy server host:port" &= typ "HOST:PORT"
  , proxy = "80:80" &= help "local host to be proxy, default is 80:80" &= typ "LOCALPORT:REMOTEPORT"
  , level = "INFO" &= help "log level" &= typ "DEBUG|INFO|WARNING|ERROR"
} &= help "proxy local tcp server, mainly used for internet access local tcp server"

mode = cmdArgsMode $ modes [m_tcpSkeleton, m_tcpStub]
        &= help "hpay tool by 李明"
        &= program "hpay"
        &= summary ("hpay v"++ version)

_name = "hpayctl"

hpayctl cmd = withSocketsDo $ do
    updateGlobalLogger rootLoggerName $ setLevel $ read $ level cmd
    infoM _name $ show cmd
    case cmd of
        TcpSkeleton{..} -> tcpSkeleton hostport proxy
        TcpStub{..}     -> tcpStub hostport proxy

main :: IO ()
main = withSource $ cmdArgsRun mode >>= hpayctl

-- test code
t_skeleton  = hpayctl TcpSkeleton{hostport="127.0.0.1:9999", proxy="127.0.0.1:2222:liming/centos", level="INFO"}
t_stub      = hpayctl TcpStub{hostport="127.0.0.1:9999", proxy="3333:liming/centos", level="INFO"}