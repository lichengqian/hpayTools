{-# LANGUAGE TemplateHaskell #-}
module Constants where

import Importer
import Data.FileEmbed
import Codec.Archive.Zip
import System.Environment

type HostPort = String -- ^ host:port

sourceCodes :: ByteString
sourceCodes = $(embedFile "dist/source.zip")

unzipSources :: IO ()
unzipSources = extractFilesFromArchive [OptRecursive, OptDestination "hpayTools"] . toArchive . fromStrict $ sourceCodes

withSource :: IO () -> IO ()
withSource action = do
    args <- getArgs
    case args of
        ["----source"]  -> unzipSources
        _           -> action
