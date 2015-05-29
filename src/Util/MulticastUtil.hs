{-# LANGUAGE RecordWildCards #-}
module Util.MulticastUtil where

-- import qualified Util.MulticastUtil as Multicast
import Network.Socket
import Network.Multicast

_multicastIP = "224.0.0.99"
_multicastPort = 1230

data Sender = Sender {
    send :: String -> IO ()
}

data Receiver = Receiver {
    recv :: IO String
}

newSender :: IO Sender
newSender = do
    (sock, addr) <- multicastSender _multicastIP _multicastPort
    let send s = sendTo sock s addr >> return ()
    return Sender{..}

newReceiver :: IO Receiver
newReceiver = do
    sock <- multicastReceiver _multicastIP _multicastPort
    let recv = do
            (msg, _, _) <- recvFrom sock 1024
            return msg
    return Receiver{..}

-- test code
tSend = withSocketsDo $ do
    Sender{..} <- newSender
    send "hello world"

tRecv = withSocketsDo $ do
    Receiver{..} <- newReceiver
    msg <- recv
    print msg