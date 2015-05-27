{-# LANGUAGE PatternGuards  #-}
module Util.ConfigUtil where

import Data.Map as M
import Importer
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Multicast

multicastHost :: HostName
multicastHost = "224.0.0.99"

multicastPort :: PortNumber
multicastPort = 9999

senderSocket = multicastSender multicastHost multicastPort
receiverSocket = multicastReceiver multicastHost multicastPort

sendBinaryTo sock addr' a= void $ sendTo sock (encodeStrict a) addr'

recvBinaryFrom sock = do
    (msg, addr) <- recvFrom sock 1024
    return (decodeStrict msg, addr)

queryConfig :: Binary a => String -> String -> IO a
queryConfig who key = do
    (sock, addr') <- senderSocket
    sendBinaryTo sock addr' (who, key)
    (msg, _) <- recvBinaryFrom sock
    return msg

servConfig :: Socket -> String -> Map String ByteString -> IO ()
servConfig sock who entrys = forever $ do
    ((who', key), addr) <- recvBinaryFrom sock
    let response "*" = sendBinaryTo sock addr entrys
        response key
            | Just v <- M.lookup key entrys = sendBinaryTo sock addr v
            | otherwise = return ()
    when (who == who' && member key entrys) $ response key
