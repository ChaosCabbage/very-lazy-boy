module CPU
    ( 
    ) where

import Control.Monad.ST as ST
import Data.Word
import Data.Array.ST

type Register8 = Word8
type Register16 = Word16
type Address = Word16
type Opcode = Word8

-- Memory for use in the ST monad.
-- Stateful array of bytes, addressed by Address.
type STMemory s = STUArray s Address Word8

-- The whole state of the CPU and all the memory.
-- Mutable, for use in the ST monad.
data CPUEnvironment s = CPUEnvironment {
    pc :: Register16
  , rom :: STMemory s  
}

initCPU :: ST s (CPUEnvironment s)
initCPU = do
    mem <- newArray (0x00, 0x3FFF) 0x00
    return CPUEnvironment { pc = 0x100, rom = mem }

readPC :: (CPUEnvironment s) -> ST s Word16
readPC = return . pc

writePC :: Word16 -> (CPUEnvironment s) -> ST s (CPUEnvironment s)
writePC w cpu = return cpu { pc = w }

incrementPC :: (CPUEnvironment s) -> ST s (CPUEnvironment s)
incrementPC cpu = do
    p <- readPC cpu
    writePC (p+1) cpu

readMem :: Address -> (CPUEnvironment s) -> ST s Word8
readMem addr cpu = 
    readArray (rom cpu) addr

fetch :: (CPUEnvironment s) -> ST s Opcode
fetch cpu = do
    p <- readPC cpu
    incrementPC cpu
    readMem p cpu


