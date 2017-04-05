module CPU.Environment (
    CPUEnvironment(..)
  , ComboRegister(..)
  , STMemory
    ) where
    
import CPU.Types

import Data.Word
import Data.STRef
import Data.Array.ST


-- Memory for use in the ST monad.
-- Stateful array of bytes, addressed by Address.
type STMemory s = STUArray s Address Word8

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

data ComboRegister = AF | BC | DE | HL
