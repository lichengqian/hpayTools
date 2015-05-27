{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Command (
    Command(..), registerCommand, execCommand
)where
import Data.Dynamic
import Data.Map.Strict as Map
import Control.Concurrent.STM
import System.IO.Unsafe

class Typeable a => Command a where
    type Response a

{-# NOINLINE _commandRegistry #-}
_commandRegistry :: TVar (Map TypeRep Dynamic)
_commandRegistry = unsafePerformIO $ newTVarIO Map.empty

registerCommand :: (Command a, Typeable (Response a)) => a -> (a -> IO (Response a)) -> IO ()
registerCommand cmd handler = atomically $ modifyTVar _commandRegistry $ Map.insert (typeOf cmd) $ toDyn handler

execCommand :: (Command a, Typeable (Response a)) => a -> IO (Response a)
execCommand cmd = do
    maps <- readTVarIO _commandRegistry
    let Just h = Map.lookup (typeOf cmd) maps
        Just action = fromDynamic $ dynApp h (toDyn cmd)
    action


