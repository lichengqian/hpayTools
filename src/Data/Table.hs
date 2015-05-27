{-# LANGUAGE MonadComprehensions #-}
module Data.Table (
    Table,
    newTableIO, tableSetID,tableNewID, tableSize,
    tableAdd, tableRemove, tableLookup, tableFilter,
    tableInsert, toList, keys, tableValues,
    module Control.Concurrent.STM
)where

import qualified Data.HashMap.Strict as M
import Control.Concurrent.STM
import Control.DeepSeq

type Table a = TVar (Int, M.HashMap Int a)

stm = atomically

newTableIO :: IO (Table a)
newTableIO = newTVarIO (1, M.empty)

tableSetID :: Table a -> Int-> STM Int
tableSetID t id = do
    (i, cs) <- readTVar t
    writeTVar t (id, cs)
    return i

tableNewID :: Table a -> STM Int
tableNewID t = do
    (i, cs) <- readTVar t
    writeTVar t (i+1, cs)
    return i

tableSize :: Table a -> STM Int
tableSize t = do
    (_, cs) <- readTVar t
    return $ M.size cs

tableAdd :: Table a -> a -> STM Int
tableAdd t a = do
    (i, cs) <- readTVar t
    if i /= 256
        then do
            writeTVar t (i+1, M.insert i a cs)
            return i
        else do
            writeTVar t (2, M.insert 1 a cs)
            return 1

tableRemove :: Table a -> Int -> STM (Maybe a)
tableRemove t i = do
    (ni, cs) <- readTVar t
    writeTVar t (ni, M.delete i cs)
    return (M.lookup i cs)

tableLookup :: Table a -> Int -> STM (Maybe a)
tableLookup t i = [M.lookup i cs | (_, cs) <- readTVar t]

-- when idx ==0 will assign a new id
tableInsert :: Table a -> Int -> a -> STM Int
tableInsert t i a 
  | i == 0 = tableAdd t a
  | otherwise = do
    (i', cs) <- readTVar t
    writeTVar t (max (i+1) i', M.insert i a cs)
    return i
   
toList :: Table a -> STM [(Int, a)]
toList t = [M.toList cs | (i, cs) <- readTVar t]

keys :: Table a -> STM [Int]
keys t = [M.keys cs | (i, cs) <- readTVar t]

tableValues :: Table a -> STM [a]
tableValues t = [map snd vs | vs <- toList t]

tableFilter :: (NFData a) => (a -> Bool) -> Table a -> STM Int
tableFilter f t = do
    (i, m) <- readTVar t
    let m' = M.filter f m
    m' `deepseq` writeTVar t (i, m')
    return $ M.size m'
