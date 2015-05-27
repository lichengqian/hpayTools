{-# LANGUAGE ScopedTypeVariables #-}
module CloudLogger.LogClient where

import Importer

import qualified Data.Map as M
import qualified STMContainers.Map as SC
import qualified ListT

import           System.IO

import CloudLogger.Types
import CloudLogger.LogParser
import Util.LuaUtil
import Util.ShellyUtil
import Util.StreamUtil
import Util.ConfigUtil

-- logClient  : send log entry to logServer
-- logServer  : receive log entry from logClient and dispatch them to logMonitor
-- logMonitor : recieve log entry from logServer

quote s = "\"" ++ s ++ "\""

-- | 程序不关闭Socket连接，依赖操作系统关闭连接!
logClient :: String -> LuaState -> String -> String -> String -> IO ()
logClient n l name host port = do
    (inn, out) <- connectSockStream host port
    writeBinary out "logServer"
    writeBinary out $ "openLog(" ++ quote (name) ++ ")"

    let log :: LogEntry -> IO ()
        log = writeBinary out

        filterLog :: LogEntry -> IO ()
        filterLog = callproc l "filter" . toStringMap

        readLog, tailLog :: Priority -> String -> IO ()
        readLog loglevel logfile = void $ readLogs (logfile) $ \entry@(LogEntry a b c d) -> do
            when (readPriority b >= loglevel) $ filterLog entry

        tailLog loglevel logfile = void $ tail_f (logfile) $ \h -> do
            input <- hGetContents h
            msg   <- withLogs "stdin" input filterLog
            errorM n $ "exit parser!!!" ++ msg
            return ()

    registerhsfunction l "log"     $ log . fromStringMap
    registerhsfunction l "readLog" $ readLog . readPriority
    registerhsfunction l "tailLog" $ tailLog . readPriority

-- | 程序不关闭Socket连接，依赖操作系统关闭连接!
logMonitor :: LuaState -> String -> String -> IO ()
logMonitor l host port = do
    (inn, out) <- connectSockStream host port
    writeBinary out "logServer"
    let uploadScript, startMonitor :: String -> IO ()
        uploadScript = writeBinary out

        startMonitor rule = do
            writeBinary out $ "openMonitor(\"" ++ rule ++ "\")"

            fShow <- newMVar True
            -- support pause / resume monitor!!!
            hSetBuffering stdin NoBuffering
            foreverIO $ do
                getChar
                modifyMVar_ fShow $ \f -> do
                    let f' = not f
                    if f' then putStrLn "continue monitor..."
                          else putStrLn "pause monitor..., press any key to continue"
                    return f'

            forever $ do
                code <- readBinary inn
                p    <- readMVar fShow
                when p $ putStrLn code
    registerhsfunction l "uploadScript" uploadScript
    registerhsfunction l "startMonitor" startMonitor

registLogClient l = do
    let client  = "logClient"
        monitor = "logMonitor"
    registerhsfunction l client  $ logClient client l
    registerhsfunction l monitor $ logMonitor l