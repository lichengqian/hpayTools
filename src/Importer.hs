module Importer (
    stm, foreverIO,
    encodeStrict, decodeStrict,
    module X
) where

import Data.Monoid as X
import Control.Applicative as X
import Data.ByteString as X (ByteString)
import Data.ByteString.Lazy as X (fromStrict, toStrict)
import Data.Binary as X
import Control.Monad.Extra as X
import Control.Exception.Extra as X (try, try_, SomeException, finally)

import Control.Concurrent as X
import Control.Concurrent.STM as X
import GHC.Generics as X (Generic)
import Data.List.Extra as X
import System.Log.Logger as X

stm = atomically

stmIO = join . stm

foreverIO = forkIO . forever

encodeStrict :: Binary a => a -> ByteString
encodeStrict = toStrict . encode

decodeStrict :: Binary a => ByteString -> a
decodeStrict = decode . fromStrict