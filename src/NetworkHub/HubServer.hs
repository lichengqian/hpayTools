{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module NetworkHub.HubServer(hubServerApp) where

import Importer
import Data.Table
import System.IO.Channels
import qualified System.IO.Streams as Streams
import System.IO.Streams.Binary
import Util.StreamUtil
import NetworkHub.Types
import Control.Concurrent.Async
import Control.Exception

name = "hubServer"

namedAction actionname = bracket_ before after where
    before = infoM name $ "[" ++ actionname ++ "] started!"
    after  = infoM name $ "[" ++ actionname ++ "] stopped!"

-- ^ 保存所有已连接的客户端
data Server = Server {
    clients :: Table (TChan NetPackage)
  , addClient :: Int -> IO (Int, TChan NetPackage)          -- 新建Client
  , removeClient :: Int -> IO ()        -- 删除Client
  , sendPackage :: NetPackage -> IO ()  -- 发送Package
}

newServer :: IO Server
newServer = do
    clients :: Table (TChan NetPackage) <- newTableIO
    stm $ tableSetID clients 10000    -- clint id start from 10000

    let addClient ip' = do
            channel :: TChan NetPackage <- newTChanIO
            clientid <- stm $ tableInsert clients ip' channel
            sz <- stm $ tableSize clients
            infoM name $ "add hub client : " ++ show clientid ++ ", size : " ++ show sz
            return (clientid, channel)

        removeClient clientid = do
            sz <- stm $ do
                tableRemove clients clientid
                tableSize clients
            infoM name $ "remove hub client : " ++ show clientid ++ ", size : " ++ show sz

        sendPackage :: NetPackage -> IO ()
        sendPackage p@(NetPackage src dest content) = do
              mc <- stm $ tableLookup clients dest
              case mc of
                Just channel  -> stm $ writeTChan channel p
                Nothing       -> warningM name $ "discard dead message:" ++ show (src, dest)
    return Server{..}

startHubApps Server{..} apps = do
    let newClient ip' = do
            (clientid, channel) <- addClient ip'
            let sendMessage (dest, bs) = sendPackage $ NetPackage clientid dest bs
                recvMessage = do
                    NetPackage src _ bs <- stm $ readTChan channel
                    return (src, bs)
                closeClient = removeClient clientid
            return HubClient{..}

    -- 启动内置应用
    forM_ apps $ \(ip', app) -> do
        client <- newClient ip'
        forkFinally (app client) $ \_ ->removeClient (clientid client)
        return ()

hubServerApp :: [(Int, HubApp)] -> IO StreamApp
hubServerApp apps = do
    -- ^ 保存所有已连接的客户端
    server@Server{..} <- newServer
    startHubApps server apps

    return $ \inn out -> do
        -- 分配地址 0: 随机分配
        ip' <- readBinary (inn :: InputStream ByteString)
        (clientid, channel) <- addClient ip'
        let prefix = show clientid ++ ":"
            processPackage (NetPackage src 0 bs)
                | m@(HubCheckHealth 0) <- decodeStrict bs = do
                    debugM name $ "recv healty check " ++ show (src, m)
                    sendPackage $ NetPackage 0 src $ encodeStrict $ HubHealthy 0
                | m@(HubCheckHealth checkid) <- decodeStrict bs = do
                    debugM name $ "recv healty check " ++ show (src, m)
                    stm $ do
                        mc <- tableLookup clients checkid
                        case  mc of
                            Just channel' -> writeTChan channel' $ NetPackage 0 checkid $ encodeStrict $ HubCheckHealth src
                            Nothing -> writeTChan channel $ NetPackage 0 clientid $ encodeStrict $ HubClientNotfound checkid
            processPackage packet = sendPackage packet

            receive = forever $ do      -- 接收线程，根据目的地址分发报文
              packet <- readBinary inn
              processPackage packet
            serve = forever $ do
                p <- stm $ readTChan channel
                writeBinary out p
            runClient = do
                writeBinary out clientid
                race_ (namedAction (prefix ++ "receive") receive) (namedAction (prefix ++ "serv") serve)
        -- TODO: removeClient从来不会被调用！！！因为两个子线程永远不退出！
        runClient `finally` removeClient clientid