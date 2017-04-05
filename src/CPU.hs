module CPU
    ( 
        CPU,
        runCPU,
        initCPU,
        fetch,
        fetch16,

        readReg,
        writeReg,
        readComboReg,
        writeComboReg

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

readReg :: CPURegister s r -> CPU s r
readReg reg = CPU $ \cpu -> readSTRef (reg cpu)

writeReg :: CPURegister s w -> w -> CPU s ()
writeReg reg w = CPU $ \cpu -> writeSTRef (reg cpu) w

readComboReg :: ComboRegister -> CPU s Word16
readComboReg reg = do
    let (low, high) = registerPair reg
    (readReg low) `joinBytesM` (readReg high)

writeComboReg :: ComboRegister -> Word16 -> CPU s ()
writeComboReg reg w = do
    let (lowReg, highReg) = registerPair reg
    let (lowByte, highByte) = toBytes w
    writeReg lowReg lowByte
    writeReg highReg highByte


readMem :: Address -> CPU s Word8
readMem addr = CPU $ \cpu -> 
    readArray (rom cpu) addr

incrementPC :: CPU s ()
incrementPC = (readReg pc) >>= (writeReg pc) . succ

fetch :: CPU s Opcode
fetch = do
    addr <- readReg pc
    incrementPC 
    readMem addr

fetch16 :: CPU s Word16
fetch16 = fetch `joinBytesM` fetch
