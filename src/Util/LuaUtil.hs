module Util.LuaUtil (
  Priority(..), readPriority,
  FromStringMap(..), ToStringMap(..),
  Lua.registerhsfunction,
  Lua.callproc,
  Lua.LuaState,
  withLua,
  withLuaConfig,
  luamain,
  module Scripting.LuaUtils,
  module System.Log.Logger
) where

import Control.Exception
import System.Environment

import qualified Data.Map as M

import qualified Scripting.Lua as Lua
import Scripting.Lua.ConfigFile
import Scripting.LuaUtils

import System.Log
import System.Log.Logger
{-# ANN module ("HLint: ignore Redundant do"::String) #-}

readPriority :: String -> Priority
readPriority level = case level of
        "WARN" -> WARNING
        _              -> read level

-- can be configed in lua table
class FromStringMap a where
    fromStringMap :: M.Map String String -> a

-- can be converted to lua table
class ToStringMap a where
    toStringMap   :: a -> M.Map String String

setRootLevel level =
  updateGlobalLogger rootLoggerName (setLevel $ read level)

withLua f = bracket Lua.newstate Lua.close $ \l -> do
  Lua.openlibs l
  Lua.registerhsfunction l "setLevel" setRootLevel
  f l
  
withLuaConfig path = bracket (openConfig path) closeConfig

runLua path f = withLua $ \l -> do
  f l
  luaDoFile l path
  
luamain f = do
  args <- getArgs
  case args of 
    [path] -> do
      -- start path
      runLua path f
    _ -> putStrLn "please input lua config file name"
