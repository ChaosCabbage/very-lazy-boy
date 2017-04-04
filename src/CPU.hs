module CPU
    ( 
        CPU,
        runCPU,
        initCPU,
        step,

        -- Just for testing:
        readPC
    ) where

import Rom

import Control.Monad.ST as ST
import Control.Monad.Reader
import Data.Word
import Data.STRef
import Data.Array
import Data.Array.ST
import Data.Bits
import Data.Char

import Numeric (showHex)

type Register8 = Word8
type Register16 = Word16
type Address = Word16
type Opcode = Word8

-- Memory for use in the ST monad.
-- Stateful array of bytes, addressed by Address.
type STMemory s = STUArray s Address Word8
type STRegister16 s = STRef s Register16

-- The whole state of the CPU and all the memory.
-- Mutable, for use in the ST monad.
data CPUEnvironment s = CPUEnvironment {
--    a :: STRef s Register8
--  , f :: STRef s Register8
--  , b :: STRef s Register8
--  , c :: STRef s Register8
--  , d :: STRef s Register8
--  , e :: STRef s Register8
--  , h :: STRef s Register8
--  , l :: STRef s Register8
--  , sp :: STRef s Register16
    pc :: STRef s Register16
  , rom :: STMemory s  
}

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

readPC :: CPU s Word16
readPC = CPU $ \cpu -> readSTRef (pc cpu)

writePC :: Word16 -> CPU s ()
writePC w = CPU $ \cpu -> writeSTRef (pc cpu) w

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

to16 :: Word8 -> Word16
to16 = fromIntegral 

joinBytes :: Word8 -> Word8 -> Word16
joinBytes low high = 
    let low16 = to16 low;
        high16 = shiftL (to16 high) 8
    in
        high16 + low16

joinBytesM :: (Monad m) => m Word8 -> m Word8 -> m Word16
joinBytesM = liftM2 joinBytes

fetch16 :: CPU s Word16
fetch16 = fetch `joinBytesM` fetch

-- Move this into a separate file, when you can be bothered, please:

type Cycles = Int

step :: CPU s Cycles
step = fetch >>= execute

execute :: Opcode -> CPU s Cycles
execute 0x00 = nop
execute 0xC3 = fetch16 >>= jp
-- execute 0xAF = set A to zero
execute x = error $ "Unknown opcode 0x" ++ (map toUpper (showHex x ""))

nop :: CPU s Cycles
nop = return 4

jp :: Address -> CPU s Cycles
jp a = writePC a >> 
    return 16