{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Util.MulticastUtil where

-- import qualified Util.MulticastUtil as Multicast
import Network.Socket
import Network.Multicast
import Control.Monad
import Data.List.Extra

_multicastIP = "224.0.0.99"
_multicastPort = 1230

data Sender = Sender {
    ask :: String -> IO (HostName, String)
}

data Receiver = Receiver {
    answer :: (String -> IO (Maybe String)) -> IO ()
}

newSender :: IO Sender
newSender = do
    (sock, addr) <- multicastSender _multicastIP _multicastPort
    let ask s = do
            sendTo sock s addr
            (msg, _, addr) <- recvFrom sock 1024
            return (head $ splitOn ":" $ show addr, msg)
    return Sender{..}

newReceiver :: IO Receiver
newReceiver = do
    sock <- multicastReceiver _multicastIP _multicastPort
    let answer action = forever $ do
            (msg, _, addr) <- recvFrom sock 1024
            action msg >>= \case
                Just r -> void $ sendTo sock r addr
                Nothing -> return ()
    return Receiver{..}

-- test code
tSend = withSocketsDo $ do
    Sender{..} <- newSender
    ask "hpayd" >>= print

tRecv = withSocketsDo $ do
    Receiver{..} <- newReceiver
    answer $ \case
        "hpayd" -> do
            print "here"
            return $ Just "1230"
        _       -> return Nothing