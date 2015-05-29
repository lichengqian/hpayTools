{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module TcpProxy.TcpProxy(tcpSkeleton, tcpStub) where

import Importer
import Constants

import Network.Socket.Options
import qualified Data.Map as M
import Control.Concurrent.Async
import qualified System.IO.Streams as Streams
import System.IO.Streams.Network

import Util.StreamUtil
import NetworkHub.Types
import NetworkHub.HubClient

data ProxyMessage =
    NewConnection               -- ^ 新连接申请及分配
  | CloseConnection String      -- ^ 关闭连接
  | DataStream !ByteString      -- ^ 数据报文
  deriving (Show, Generic)

instance Binary ProxyMessage

pattern PPMsg src a <- (src, decodeStrict -> a :: ProxyMessage)

name = "tcpProxy"
-- ^ 一个hubClient专门负责处理一个socket连接
socketClient dest s HubClient{..} = do
    (s_inn, s_out) <- socketToClosableStreams s
    let recv = do
            mbs <- Streams.read s_inn
            case mbs of
                Just bs -> do
                    sendMessage (dest, encodeStrict $ DataStream bs)
                    recv
                Nothing -> do
                    infoM name $ "socket closed! exit recv thread!"
                    sendMessage (dest, encodeStrict $ CloseConnection "read Nothing")
                    threadDelay 500000  -- 等待半秒，确保sendMessage消息发送完成
                    closeClient         -- 关闭hub client
                    return ()
        serv = do
            PPMsg src msg <- recvMessage
            case msg of
                DataStream bs -> do
                    Streams.write (Just bs) s_out
                    serv
                CloseConnection errmsg -> do
                    infoM name $ "remote closed! exit serv thread!" ++ errmsg
                    closeSock s     -- 关闭socket将导致recv线程退出
                    return ()
    race_ serv recv `finally` do
        infoM name $ "closing socket"
        return ()

tcpPrefix = "tcp://"
tcpRegister remoteport = (1, encodeStrict $ NameRegister $ tcpPrefix ++ remoteport)
tcpQuery remoteport= (1, encodeStrict $ NameQuery $ tcpPrefix ++ remoteport)

-- 发布本地tcp server ，需要指定一个远程虚拟端口
tcpSkeleton :: HostPort -> String -> IO ()
tcpSkeleton hostport proxy = hubClient hostport $ \_ HubClient{..} -> do
    let [localhost, localport, remoteport] = splitOn ":" proxy
    -- step 1 : 注册服务名称 : tcpSkeleton/xxx
    sendMessage $ tcpRegister remoteport
    (_, bs) <- recvMessage
    case decodeStrict bs of
        NameResult (Left err) -> error $ "register name error : "
        NameResult (Right serverip) -> return ()
    infoM name $ "register ok! " ++ proxy

    -- step 2 : 只需要处理新建连接请求
    let go = forever $ do
        PPMsg src e <- recvMessage
        debugM name $ "recv " ++ show (src, e)
        case e of
            NewConnection -> do
                mret <- try_ $ connectSock localhost localport
                case mret of
                    -- 连接失败，发送通知
                    Left err -> sendMessage (src, encodeStrict $ CloseConnection $ show err)
                    Right (s, addr) -> do
                        infoM name $ "connect to " ++ localhost ++ ":" ++ localport
                        -- 新启动一个hubClient
                        let go = hubClient hostport $ \_ client@HubClient{..} -> do
                                    sendMessage (src, encodeStrict NewConnection)
                                    socketClient src s client
                        forkFinally go $ \_ -> closeSock s
                        return ()
                return ()
            _ -> do
                warningM name $ "unexpected message: " ++ show (src, e)
                return ()
    go `finally` infoM name "tcpSkeleton exit!"

-- | 启动本地tcp stub，转发至远程虚拟端口
tcpStub :: HostPort -> String -> IO ()
tcpStub hostport proxy = hubClient hostport $ \hubSocket HubClient{..} -> do
    let [localport, remoteport] = splitOn ":" proxy
    -- step 1 : 查询服务名称
    sendMessage $ tcpQuery remoteport
    (_, decodeStrict -> msg) <- recvMessage
    serverip <- case msg of
        NameResult (Left err) -> error $ "register name error : " ++ err
        NameResult (Right serverip) -> return serverip
    infoM name $ "name query port " ++ remoteport ++ " = " ++ show serverip

    -- 设置Socket接收超时，用于心跳检测
    let t = 5000000     -- 5秒心跳间隔
    setSendTimeout hubSocket t
    setRecvTimeout hubSocket t

    listen HostIPv4 localport $ \(ls, _) -> do
        let go = forever $ acceptFork ls $ \(s, addr) -> do
                -- 新启动一个hubClient
                infoM name $ "income connection from port : " ++ localport
                hubClient hostport $ \_ client@HubClient{..} -> do
                    -- 申请虚拟IP
                    sendMessage (serverip, encodeStrict NewConnection)
                    PPMsg src msg <- recvMessage
                    case msg of
                        NewConnection -> socketClient src s client
                        CloseConnection errmsg -> do
                            warningM name $ "connect failed ! " ++ errmsg
                            return ()

                infoM name $ "income connection closed " ++ show addr
            heartbeat t = do
                threadDelay t
                debugM name $ "send heartbeat to :" ++ show serverip
                -- TODO: 如果hubServer宕机，sendMessage会阻塞！！?，导致程序永不退出！
                sendMessage (0, encodeStrict $ HubCheckHealth serverip)
                mout <- try_ recvMessage
                debugM name $ "recv heartbeat response : " ++ show mout
                case mout of
                    Right (_, decodeStrict -> HubHealthy _) -> heartbeat t
                    Right (_, decodeStrict -> HubClientNotfound _) -> warningM name $ "notfound : " ++ show serverip
                    Left e -> warningM name $ "error : " ++ show e
        race_ go $ heartbeat 5000000 `finally` do
            warningM name $ "no heartbeat response, stop listen on " ++ localport
            closeSock ls       -- 5秒心跳一次