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
import System.Random
import qualified System.IO.Streams as Streams
import System.IO.Streams.Binary
import Util.StreamUtil
import Network.Socket.Options

import NetworkHub.Types
import Control.Concurrent.Async

name = "hubClient"

-- | 程序不关闭Socket连接，依赖操作系统关闭连接!
hubClient :: HostPort -> (Socket -> HubApp) -> IO ()
hubClient hostport app = connect proxyhost proxyport go where
    [proxyhost, proxyport] = splitOn ":" hostport
    go (connectionSocket, remoteAddr) = do
        infoM name $ "Connection established to " ++ show remoteAddr
        (inn, out') <- socketToClosableStreams connectionSocket
        out  <- lockingOutputStream out'
        writeBinary out "hubServer"
        let register = do
                ip' <- randomIO :: IO ClientID
                writeBinary out ip'
                er :: Either String ClientID <- readBinary inn
                case er of
                    Left err -> infoM name err >> register
                    Right ip -> return ip
        -- 注册客户端ID
        clientid <- register
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

