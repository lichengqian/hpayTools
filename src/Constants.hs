{-# LANGUAGE TemplateHaskell #-}
module Constants where

import Importer
import Util.ZipUtil

type HostPort = String -- ^ host:port

sourceCodes :: ByteString
sourceCodes = $(createZip ["Setup.hs", "hpayTools.cabal"])
