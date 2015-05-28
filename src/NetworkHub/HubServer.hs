{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module NetworkHub.HubServer(hubServerApp) where

import Importer
import System.IO.Channels
import qualified System.IO.Streams as Streams
import System.IO.Streams.Binary
import Util.StreamUtil
import NetworkHub.Types
import Control.Concurrent.Async
import Control.Exception
import qualified Data.HashMap.Strict as M

name = "hubServer"

namedAction actionname = bracket_ before after where
    before = infoM name $ "[" ++ actionname ++ "] started!"
    after  = infoM name $ "[" ++ actionname ++ "] stopped!"

type TClients = TVar (M.HashMap ClientID (TChan NetPackage))
-- ^ 保存所有已连接的客户端
data Server = Server {
    clients :: TClients
  , addClient :: ClientID -> STM (Maybe (TChan NetPackage))          -- 新建Client
  , removeClient :: ClientID -> IO ()        -- 删除Client
  , sendPackage :: NetPackage -> IO ()  -- 发送Package
}

newServer :: IO Server
newServer = do
    clients :: TClients <- stm $ newTVar M.empty

    let addClient ip' = do
            cs <- readTVar clients
            case M.lookup ip' cs of
                Just _ -> return Nothing
                Nothing -> do
                    channel :: TChan NetPackage <- newTChan
                    writeTVar clients $ M.insert ip' channel cs
                    return $ Just channel

--            clientid <- stm $ tableInsert clients ip' channel
--            sz <- stm $ tableSize clients
--            infoM name $ "add hub client : " ++ show clientid ++ ", size : " ++ show sz
--            return (clientid, channel)

        removeClient clientid = do
            sz <- stm $ do
                cs <- readTVar clients
                let cs' = M.delete clientid cs
                writeTVar clients cs'
                return $ M.size cs'
            infoM name $ "remove hub client : " ++ show clientid ++ ", size : " ++ show sz

        sendPackage :: NetPackage -> IO ()
        sendPackage p@(NetPackage src dest content) = do
              mc <- stm $ do
                cs <- readTVar clients
                return $ M.lookup dest cs
              case mc of
                Just channel  -> stm $ writeTChan channel p
                Nothing       -> warningM name $ "discard dead message:" ++ show (src, dest)
    return Server{..}

startHubApps Server{..} apps = do
    let newClient clientid = do
            Just channel <- stm $ addClient clientid
            let sendMessage (dest, bs) = sendPackage $ NetPackage clientid dest bs
                recvMessage = do
                    NetPackage src _ bs <- stm $ readTChan channel
                    return (src, bs)
                closeClient = removeClient clientid
            return HubClient{..}

    -- 启动内置应用
    forM_ apps $ \(ip', app) -> do
        client <- newClient ip'
        forkFinally (app client) $ \_ ->closeClient client
        return ()

hubServerApp :: [(Int, HubApp)] -> IO StreamApp
hubServerApp apps = do
    -- ^ 保存所有已连接的客户端
    server@Server{..} <- newServer
    startHubApps server apps

    return $ \inn out -> do
        -- 分配地址 0: 随机分配
        let checkAddClient = do
                ip'<- readBinary (inn :: InputStream ByteString)
                mc <- stm $ addClient ip'
                case mc of
                    Just channel -> writeBinary out (Right ip' :: Either String ClientID) >> return (ip', channel)
                    Nothing      -> writeBinary out (Left "already used clientid" :: Either String ClientID) >> checkAddClient
        (clientid, channel) <- checkAddClient
        let prefix = show clientid ++ ":"
            processPackage (NetPackage src 0 bs)
                | m@(HubCheckHealth 0) <- decodeStrict bs = do
                    debugM name $ "recv healty check " ++ show (src, m)
                    sendPackage $ NetPackage 0 src $ encodeStrict $ HubHealthy 0
                | m@(HubCheckHealth checkid) <- decodeStrict bs = do
                    debugM name $ "recv healty check " ++ show (src, m)
                    stm $ do
                        cs <- readTVar clients
                        case  M.lookup checkid cs of
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