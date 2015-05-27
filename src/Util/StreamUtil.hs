{-# LANGUAGE MonadComprehensions #-}
module Util.StreamUtil (
  InputStream, OutputStream, lockingOutputStream,
  toStrict, readBinary, writeBinary,
  socketToStreams, socketToClosableStreams,
  StreamApp, withSocketStream, withConsoleStream, connectSockStream,
  module Network.Simple.TCP
)where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy  (toStrict)

import           System.IO.Streams (InputStream, OutputStream, lockingOutputStream, socketToStreams)
import qualified System.IO.Streams as Streams
import Data.Maybe
import Data.Binary
import System.IO.Streams.Binary

import Network.Simple.TCP

type StreamApp = InputStream BS.ByteString -> OutputStream BS.ByteString -> IO ()

readBinary :: Binary a => InputStream BS.ByteString -> IO a
readBinary inn = fmap fromJust $ binaryFromStream inn

writeBinary :: Binary a => OutputStream BS.ByteString -> a -> IO ()
writeBinary out = binaryToStream out . Just

-- close the socket when recv Nothing
closableStream out s = Streams.makeOutputStream f where
    f Nothing = closeSock s
    f a       = Streams.write a out

socketToClosableStreams s =
    [(inn, out) | (inn, out') <- socketToStreams s, out <- closableStream out' s]

-- | run StreamApp in a socket connection env
--   note : OutputStream support close socket!!!
withSocketStream :: StreamApp -> (Socket, SockAddr) -> IO ()
withSocketStream app (connectionSocket, remoteAddr) = do
    putStrLn $ "Connection established to " ++ show remoteAddr
    (inn, out) <- socketToClosableStreams connectionSocket
    app inn out

connectSockStream host port = do
    (s, _) <- connectSock host port
    socketToClosableStreams s

withConsoleStream :: StreamApp -> IO ()
withConsoleStream app = app Streams.stdin Streams.stdout