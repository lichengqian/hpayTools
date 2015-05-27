{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module CloudLogger.LogServer(logServerApp) where

import Importer

import            System.IO
import qualified Scripting.Lua as Lua
import qualified System.IO.Streams as Streams
import Control.Concurrent.Async

import Util.LuaUtil
import Util.ShellyUtil
import Util.StreamUtil
import Util.ConfigUtil

import CloudLogger.Types

tryAllException :: IO a -> IO (Either SomeException a)
tryAllException = try

-- | Log服务器状态（全局）
data LogServer = LogServer {
    name        :: String,                     -- ^ global log queue. send all log message to this queue
    logQueue    :: TChan (String, LogEntry)    -- ^ broadcast log channel
}

newLogServer name = LogServer name <$> newBroadcastTChanIO

-- | 代表一个连接的Log客户端(logMonitor或者logClient)
data LogClient = LogClient {
    inn :: InputStream ByteString,
    out :: OutputStream ByteString,
    l   :: LuaState
}

newLogClient inn out = do
    l <- Lua.newstate
    Lua.openlibs l
    return LogClient{..}

openLog :: LogClient -> LogServer -> String -> IO ()
openLog LogClient{..} LogServer{..} logName = do
    infoM name $ "openLog " ++ logName

    -- recv log entry and process it
    tryAllException $ forever $ do
        entry <- readBinary inn
        debugM name $ "in haskell:" ++ logName ++ show entry
        stm $ writeTChan logQueue (logName, entry)

    infoM name $ "closing log client " ++ logName
    return ()

openMonitor :: LogClient -> LogServer -> String -> IO ()
openMonitor LogClient{..} LogServer{..} ruleFile = do
    infoM name $ "openMonitor " ++ ruleFile
    -- load rule file, in which define filter function
    unless (null ruleFile) $ do
        Lua.loadfile l $ "monitor_rules/" ++ ruleFile
        Lua.call l 0 0

    registerhsfunction l "print" $ \s -> writeBinary out (s :: String)

    channel <- stm $ dupTChan logQueue
    --log message dispatch thread
    tryAllException $ race_ (Streams.read inn) $ forever $ do
          (source, entry) <- stm $ readTChan channel
          callproc l "filter" source $ toStringMap entry

    infoM name "monitor dispatcher thread exist!!!!!!!!!!!!!!!!"
    return ()

logServerApp :: IO StreamApp
logServerApp = do
    server <- newLogServer "logServer"
    return $ \inn out -> do
        client@LogClient{..} <- newLogClient inn out

        registerhsfunction l "openLog"     $ openLog     client server
        registerhsfunction l "openMonitor" $ openMonitor client server

        let f = forever $ do        -- ^ recv lua script
            script <- readBinary inn
            debugM "logServer" script  -- NEED A SWITCH!!!
            tryAllException $ luaDoString l script

        f `finally` Lua.close l
