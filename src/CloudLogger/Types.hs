{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CloudLogger.Types where
import qualified Data.Map     as M
import           Importer
import           Util.LuaUtil

data LogEntry = LogEntry {
    logTime  :: String,
    logLevel :: String,
    logTag   :: String,
    logMsg   :: String
} deriving (Show, Generic)

instance Binary LogEntry

instance FromStringMap LogEntry where
    fromStringMap m = LogEntry{..} where
        Just logTime  = M.lookup "time"  m
        Just logLevel = M.lookup "level" m
        Just logTag   = M.lookup "tag"   m
        Just logMsg   = M.lookup "message"   m

instance ToStringMap LogEntry where
    toStringMap LogEntry{..} = f M.empty where
        f = M.insert "time" logTime .
            M.insert "level" logLevel .
            M.insert "tag" logTag .
            M.insert "message" logMsg
