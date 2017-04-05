module CPU.Environment (
    CPUEnvironment(..)
  , ComboRegister(..)
  , STMemory
  , CPURegister
  , initCPU
  , registerPair
    ) where
    
import CPU.Types
import Rom

import Data.Word
import Data.STRef
import Data.Array.ST
import Control.Monad.ST as ST


-- Memory for use in the ST monad.
-- Stateful array of bytes, addressed by Address.
type STMemory s = STUArray s Address Word8
type CPURegister s w = (CPUEnvironment s -> STRef s w)

-- The whole state of the CPU and all the memory.
-- Mutable, for use in the ST monad.
data CPUEnvironment s = CPUEnvironment {
    a :: STRef s Register8
  , f :: STRef s Register8
  , b :: STRef s Register8
  , c :: STRef s Register8
  , d :: STRef s Register8
  , e :: STRef s Register8
  , h :: STRef s Register8
  , l :: STRef s Register8
  , sp :: STRef s Register16
  , pc :: STRef s Register16
  , rom :: STMemory s  
}

data ComboRegister = AF | BC | DE | HL

initCPU :: Rom -> ST s (CPUEnvironment s)
initCPU rom = do
    rom <- thaw rom
    a <- newSTRef 0x00
    f <- newSTRef 0x00
    b <- newSTRef 0x00
    c <- newSTRef 0x00
    d <- newSTRef 0x00
    e <- newSTRef 0x00
    h <- newSTRef 0x00
    l <- newSTRef 0x00
    sp <- newSTRef 0x00
    pc <- newSTRef 0x100
    return CPUEnvironment { 
        a = a,
        f = f,
        b = b,
        c = c,
        d = d,
        e = e,
        h = h,
        l = l,
        sp = sp,
        pc = pc, 
        rom = rom 
    }

registerPair :: ComboRegister -> (CPURegister s Word8, CPURegister s Word8)  
registerPair AF = (a, f)
registerPair BC = (b, c)
registerPair DE = (d, e)
registerPair HL = (h, l)

