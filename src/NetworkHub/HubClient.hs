{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}

module NetworkHub.HubClient (
    hubClient
) where

import Importer
import Constants
import Data.Table
import System.IO.Channels
import qualified System.IO.Streams as Streams
import System.IO.Streams.Binary
import Util.StreamUtil
import Network.Socket.Options

import NetworkHub.Types
import Control.Concurrent.Async

name = "hubClient"

-- | 程序不关闭Socket连接，依赖操作系统关闭连接!
hubClient :: HostPort -> Int -> (Socket -> HubApp) -> IO ()
hubClient hostport ip' app = connect proxyhost proxyport go where
    [proxyhost, proxyport] = splitOn ":" hostport
    go (connectionSocket, remoteAddr) = do
        -- 设置NoDelay，防止socket关闭时丢包
        setTcpNoDelay connectionSocket True
        infoM name $ "Connection established to " ++ show remoteAddr
        (inn, out') <- socketToClosableStreams connectionSocket
        out  <- lockingOutputStream out'
        writeBinary out "hubServer"
        -- 申请客户端ID
        clientid <- writeBinary out ip' >> readBinary inn
        infoM name $ "hub client id is " ++ show clientid

        let sendMessage (dest, bs) = writeBinary out $ NetPackage clientid dest bs
            recvMessage = do
                p@(NetPackage src _ bs) <- readBinary inn
                case (src, bs) of
                    (0, decodeStrict -> HubCheckHealth fromid) -> do
                        debugM name $ "recv health check from " ++ show fromid
                        sendMessage (fromid, encodeStrict $ HubHealthy clientid)
                        recvMessage
                    _ -> return (src, bs)
            closeClient = do
                closeSock connectionSocket
                infoM name $ "hub client closed " ++ show clientid
        -- 执行App
        app connectionSocket HubClient{..}

