{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module NetworkHub.Types where
import Importer
import Util.StreamUtil

type SrcPort = Int
type DestPort = Int

data NetPackage = NetPackage !SrcPort !DestPort !ByteString -- ^ source dest content
    deriving (Generic)

instance Binary NetPackage

-- hub client和server之间的通信消息，hub server的DestPort固定为0
data HubMessage = HubClientRegister Int     -- client注册，分配ID,暂不使用
                | HubCheckHealth    Int     -- 心跳检测请求
                | HubHealthy        Int     -- 心跳检测结果正常
                | HubClientNotfound      Int     -- 心跳检测失败，找不到对应的client
    deriving (Show, Generic)
instance Binary HubMessage

-- 代表一个抽象的Hub客户端
data HubClient = HubClient {
    clientid :: Int                         -- ^ 客户端唯一ID
  , recvMessage :: IO (SrcPort, ByteString)          -- ^ 消息接收
  , sendMessage :: (DestPort, ByteString) -> IO ()   -- ^ 消息发送
  , closeClient :: IO ()                            -- 强制关闭client
}

-- 代表一个Hub 应用
type HubApp = HubClient -> IO ()

-- 命名服务消息
data NameMessage = NameRegister !String        -- ^ 返回Either String ()
                 | NameQuery !String                -- ^ 返回Either String Int
    deriving (Generic, Show)

instance Binary NameMessage

pattern PNameMessage src a <- (src, decodeStrict -> a)
