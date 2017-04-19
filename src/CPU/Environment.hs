module CPU.Environment (
    CPUEnvironment(..)
  , ComboRegister(..)
  , MemoryBank
  , CPURegister
  , initCPU
  , registerPair
    ) where
    
import CPU.Types
import CPU.FrozenEnvironment

import Data.Word
import Data.STRef
import Data.Array.ST
import Control.Monad.ST as ST

type MemoryBank s = (CPUEnvironment s -> STUArray s Address Word8)
type CPURegister s w = (CPUEnvironment s -> STRef s w)

-- The whole state of the CPU and all the memory.
-- Mutable, for use in the ST monad.
data CPUEnvironment s = CPUEnvironment {
    -- Registers:
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
  -- Memory banks:
  , rom00 :: STUArray s Address Word8
  , rom01 :: STUArray s Address Word8
  , vram :: STUArray s Address Word8
  , extram :: STUArray s Address Word8
  , wram0 :: STUArray s Address Word8
  , wram1 :: STUArray s Address Word8
  , oam :: STUArray s Address Word8
  , ioports :: STUArray s Address Word8
  , hram :: STUArray s Address Word8
  , iereg :: STUArray s Address Word8
}

initCPU :: Memory -> ST s (CPUEnvironment s)
initCPU rom = resumeCPU $ defaultCPU { frz_rom00 = rom }

resumeCPU :: FrozenCPUEnvironment -> ST s (CPUEnvironment s)
resumeCPU state = do
    rom00   <- thaw $ frz_rom00 state
    rom01   <- thaw $ frz_rom01 state
    vram    <- thaw $ frz_vram state
    extram  <- thaw $ frz_extram state
    wram0   <- thaw $ frz_wram0 state
    wram1   <- thaw $ frz_wram1 state
    oam     <- thaw $ frz_oam state
    ioports <- thaw $ frz_ioports state
    hram    <- thaw $ frz_hram state
    iereg   <- thaw $ frz_iereg state
    a  <- newSTRef $ frz_a state
    f  <- newSTRef $ frz_f state
    b  <- newSTRef $ frz_b state
    c  <- newSTRef $ frz_c state
    d  <- newSTRef $ frz_d state
    e  <- newSTRef $ frz_e state
    h  <- newSTRef $ frz_h state
    l  <- newSTRef $ frz_l state
    sp <- newSTRef 0x00
    pc <- newSTRef 0x100
    return CPUEnvironment { 
        a = a, f = f, b = b, c = c, d = d, e = e, h = h, l = l, sp = sp, pc = pc 
      , rom00 = rom00, rom01 = rom01, vram = vram, extram = extram
      , wram0 = wram0, wram1 = wram1, oam = oam, ioports = ioports, hram = hram
      , iereg = iereg
    }

--freezeCPU :: CPUEnvironment s -> ST s FrozenCPUEnvironment
--freezeCPU cpu = 

-- Each pair of 8-bit registers can be accessed 
-- as a single 16-bit register.
data ComboRegister = AF | BC | DE | HL

registerPair :: ComboRegister -> (CPURegister s Word8, CPURegister s Word8)  
registerPair AF = (a, f)
registerPair BC = (b, c)
registerPair DE = (d, e)
registerPair HL = (h, l)

