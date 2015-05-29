{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module NetworkHub.HubServer(hubServerApp) where

import Importer
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

data Client = Client {
    clientid :: ClientID
  , channel :: TChan NetPackage
  , writePackage :: NetPackage -> STM ()
}

type TClients = TVar (M.HashMap ClientID Client)
-- ^ 保存所有已连接的客户端
data Server = Server {
    addClient :: ClientID -> STM (Maybe Client)          -- 新建Client
  , removeClient :: ClientID -> IO ()        -- 删除Client
  , lookupClient :: ClientID -> STM (Maybe Client)  --查找Client
  , sendPackage :: NetPackage -> IO ()  -- 发送Package
  , startApp    :: ClientID -> HubApp -> IO ()      -- 启动内置应用
}

newServer :: IO Server
newServer = do
    clients :: TClients <- newTVarIO M.empty
    let
        lookupClient clientid = do
            cs <- readTVar clients
            return $ M.lookup clientid cs

        addClient clientid = do
            cs <- readTVar clients
            case M.lookup clientid cs of
                Just _ -> return Nothing
                Nothing -> do
                    channel :: TChan NetPackage <- newTChan
                    let writePackage = writeTChan channel
                        client = Client{..}
                    writeTVar clients $ M.insert clientid client cs
                    return $ Just client

        removeClient clientid = do
            sz <- stm $ do
                cs <- readTVar clients
                let cs' = M.delete clientid cs
                writeTVar clients cs'
                return $ M.size cs'
            infoM name $ "remove hub client : " ++ show clientid ++ ", size : " ++ show sz

        sendPackage p@(NetPackage src dest content) = do
              mc <- stm $ do
                cs <- readTVar clients
                return $ M.lookup dest cs
              case mc of
                Just Client{writePackage}  -> stm $ writePackage p
                Nothing       -> warningM name $ "discard dead message:" ++ show (src, dest)

        newClient clientid = do
            Just Client{..} <- stm $ addClient clientid
            let sendMessage (dest, bs) = sendPackage $ NetPackage clientid dest bs
                recvMessage = do
                    NetPackage src _ bs <- stm $ readTChan channel
                    return (src, bs)
                closeClient = removeClient clientid
            return HubClient{..}

        startApp clientid app = do
            client <- newClient clientid
            forkFinally (app client) $ \_ ->closeClient client
            return ()
    return Server{..}

hubServerApp :: [(ClientID, HubApp)] -> IO StreamApp
hubServerApp apps = do
    -- ^ 保存所有已连接的客户端
    Server{..} <- newServer
    -- 启动内置应用
    forM_ apps $ uncurry startApp

    return $ \inn' out' -> do
        inn <- Streams.lockingInputStream inn'
        out <- Streams.lockingOutputStream out'
        -- 分配地址 0: 随机分配
        let checkAddClient = do
                ip'<- readBinary inn
                case () of
                  _ | ip' < 10000 -> retry "too small clientid"
                    | otherwise  -> do
                        mc <- stm $ addClient ip'
                        case mc of
                            Just client -> writeBinary out (Right ip' :: Either String ClientID) >> return client
                            Nothing      -> retry "already used clientid"
                where
                    retry msg = writeBinary out (Left msg :: Either String ClientID) >> checkAddClient
        Client{..} <- checkAddClient
        let prefix = show clientid ++ ":"
            processPackage (NetPackage src 0 bs)
                | m@(HubCheckHealth 0) <- decodeStrict bs = do
                    debugM name $ "recv healty check " ++ show (src, m)
                    sendPackage $ NetPackage 0 src $ encodeStrict $ HubHealthy 0
                | m@(HubCheckHealth checkid) <- decodeStrict bs = do
                    debugM name $ "recv healty check " ++ show (src, m)
                    stm $ do
                        mclient <- lookupClient checkid
                        case  mclient of
                            Just Client{writePackage} -> writePackage $ NetPackage 0 checkid $ encodeStrict $ HubCheckHealth src
                            Nothing -> writePackage $ NetPackage 0 clientid $ encodeStrict $ HubClientNotfound checkid
            processPackage packet = sendPackage packet

            receive = forever $ do      -- 接收线程，根据目的地址分发报文
                (dest, bs) <- readBinary inn
                debugM name $ "received : " ++ show (dest, bs)
                processPackage $ NetPackage clientid dest bs
            serve = forever $ do
                NetPackage src dest bs <- stm $ readTChan channel
                debugM name $ "sending : " ++ show (src, dest)
                writeBinary out (src,bs)
            runClient = race_ (namedAction (prefix ++ "receive") receive) (namedAction (prefix ++ "serv") serve)
        -- TODO: removeClient从来不会被调用！！！因为两个子线程永远不退出！
        runClient `finally` removeClient clientid