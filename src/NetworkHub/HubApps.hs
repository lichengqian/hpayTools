{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module NetworkHub.HubApps(nameService) where

import Importer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO.Channels
import qualified System.IO.Streams as Streams
import System.IO.Streams.Binary
import Util.StreamUtil
import NetworkHub.Types

name = "nameService"

-- 命名服务
nameService :: HubApp
nameService HubClient{..} = do
    registry :: TVar (Map String ClientID) <- newTVarIO Map.empty

    forever $ do
        PNameMessage src msg <- recvMessage
        let response ret = do
                debugM name $ show (src, msg) ++ " --> " ++ show (ret :: Either String ClientID)
                sendMessage (src, encodeStrict ret)
        join $ stm $ case msg of
            NameRegister name -> do
                names <- readTVar registry
                case Map.lookup name names of
                    Nothing -> do
                        writeTVar registry (Map.insert name src names)
                        return $ response (Right src)
                    Just oldid -> do
                        writeTVar registry (Map.insert name src names)
                        return $ do
                            warningM name $ "覆盖命名！old:" ++ show oldid ++ ", " ++ show (name, src)
                            response (Right src)
            NameQuery    name -> do
                names <- readTVar registry
                case Map.lookup name names of
                    Nothing -> return $ response $ Left "not found"
                    Just ip -> return $ response $ Right ip
