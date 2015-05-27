module System.IO.Channels (
  OutputChannel, newOutputChannel,
  addOutputStream, removeOutputStream,
  writeStream, writeAllStream,
  module Data.Table
) where

import Control.Concurrent.STM
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import Control.Exception.Extra  --(try, SomeException)
import Data.Table

-- just contain a dynamic list of OutputStream
-- you can add / remove OutputStream at any time!
type OutputChannel b = Table (OutputStream b)

newOutputChannel :: IO (OutputChannel b)
newOutputChannel = newTableIO

addOutputStream    :: OutputChannel b -> Int -> OutputStream b -> IO Int
addOutputStream channel id out = stm $ tableInsert channel id out

removeOutputStream :: OutputChannel b -> Int -> IO ()
removeOutputStream channel id = do
    stm $ tableRemove channel id
    return ()

writeStream       :: OutputChannel b -> Int -> Maybe b -> IO ()
writeStream  channel id mp = do
    mout <- stm $ tableLookup channel id   
    case mout of
      Just out -> Streams.write mp out
      Nothing  -> return ()

writeAllStream     :: OutputChannel b -> Maybe b -> IO ()
writeAllStream channel mp = do
    os <- stm $ tableValues channel
    mapM_ (try_ . Streams.write mp) os
    
stm = atomically