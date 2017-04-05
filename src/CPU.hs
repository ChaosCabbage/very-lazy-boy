module CPU
    ( 
        CPU,
        runCPU,
        initCPU,
        fetch,
        fetch16,

        readPC,
        writePC

    ) where

import Rom
import CPU.Types
import CPU.Environment
import BitTwiddling

import Control.Monad.ST as ST
import Data.STRef
import Data.Array.ST
import Control.Monad.Reader
import Data.Word

-- CPU computations are 
-- functions from a shared stateful environment into state transformers.
newtype CPU s a = CPU { runCPU :: (CPUEnvironment s) -> ST s a }

instance Monad (CPU s) where
    return x = 
        CPU $ \_ -> return x

    m >>= f = 
        CPU $ \cpu -> do            -- This is now inside the (ST s a) monad.
            current <- runCPU m cpu -- Get the current answer of the ST.
            runCPU (f current) cpu  -- Apply the answer to f. Compose the results.        

instance Applicative (CPU s) where
    pure = return
    (<*>) = ap

instance Functor (CPU s) where
    fmap f m = 
        m >>= return . f

initCPU :: Rom -> ST s (CPUEnvironment s)
initCPU rom = do
    rom <- thaw rom
    pc <- newSTRef 0x100
    return CPUEnvironment { pc = pc, rom = rom }


readReg :: (CPUEnvironment s -> STRef s r) -> CPU s r
readReg reg = CPU $ \cpu -> readSTRef (reg cpu)

writeReg :: (CPUEnvironment s -> STRef s w) -> w -> CPU s ()
writeReg reg w = CPU $ \cpu -> writeSTRef (reg cpu) w

readPC :: CPU s Word16
readPC = readReg pc

writePC :: Word16 -> CPU s ()
writePC = writeReg pc

incrementPC :: CPU s ()
incrementPC = readPC >>= (writePC . succ)

readMem :: Address -> CPU s Word8
readMem addr = CPU $ \cpu -> 
    readArray (rom cpu) addr

fetch :: CPU s Opcode
fetch = do
    addr <- readPC
    incrementPC 
    readMem addr

fetch16 :: CPU s Word16
fetch16 = fetch `joinBytesM` fetch
