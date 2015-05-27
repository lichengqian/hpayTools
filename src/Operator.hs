{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Operator (
    module Control.Monad.IO.Class,
    module Operator
)where
import Control.Monad.Operational
import Control.Monad.IO.Class

type CardNo = String
type CardType = String
type Amount = Int

data HpayInstruction a where
    CheckCardBin :: CardNo -> CardType -> HpayInstruction Bool
    Transfer     :: Int -> HpayInstruction Bool

type HpayProgram a = ProgramT HpayInstruction IO a

transfer = singleton . Transfer
checkCardBin n t = singleton $ CheckCardBin n t

interpretT :: (forall a. HpayInstruction a -> IO a) -> HpayProgram a -> IO a
interpretT f p = viewT p >>= eval where
    eval :: ProgramViewT HpayInstruction IO a -> IO a
    eval (Return a) = return a
    eval (ins :>>= is) = do
        r <- f ins
        interpretT f (is r)

interpret :: HpayProgram a -> IO a
interpret = interpretT f

f :: HpayInstruction a -> IO a
f (CheckCardBin n t) = do
    putStrLn n
    return True
f (Transfer amount) = do
    putStrLn $ "transfer" ++ show amount
    return True

program :: HpayProgram ()
program = do
    checkCardBin "1001" "1"
    transfer 100
    liftIO $ print "io action!"
    return ()

-- | test PASS!
test = interpret program
