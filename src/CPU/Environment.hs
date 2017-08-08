module CPU.Environment (
    CPUEnvironment(..)
  , ComboRegister(..)
  , MemoryBank
  , CPURegister
  , initCPU
  , resumeCPU
  , pauseCPU
  , registerPair
    ) where
    
import CPU.Types
import CPU.FrozenEnvironment

import Data.Word
import Data.STRef
import Data.Array.ST
import Control.Monad.ST as ST

newtype MemoryBank s = MemoryBank {
    array :: (CPUEnvironment s -> STUArray s Address Word8)
}
newtype IOBank s = IOBank {
    array :: (CPUEnvironment s -> STUArray s Address Word8)
}

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
  , rom00 :: MemoryBank s
  , rom01 :: MemoryBank s
  , vram :: MemoryBank s
  , extram :: MemoryBank s
  , wram0 :: MemoryBank s
  , wram1 :: MemoryBank s
  , oam :: MemoryBank s
  , ioports :: IOBank s
  , hram :: MemoryBank s
  , iereg :: MemoryBank s
  -- Master interrupt flag
  , ime :: STRef s Bool
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
    sp <- newSTRef $ frz_sp state
    pc <- newSTRef $ frz_pc state
    ime <- newSTRef $ frz_ime state
    return CPUEnvironment { 
        a = a, f = f, b = b, c = c, d = d, e = e, h = h, l = l, sp = sp, pc = pc 
      , rom00 = MemoryBank { array=rom00 }
      , rom01 = MemoryBank { array=rom01 }
      , vram = MemoryBank { array=vram }
      , extram = MemoryBank { array=extram }
      , wram0 = MemoryBank { array=wram0 }
      , wram1 = MemoryBank { array=wram1 }
      , oam = MemoryBank { array=oam }
      , ioports = IOBank { array=ioports }
      , hram = MemoryBank { array=hram }
      , iereg = MemoryBank { array=iereg }
      , ime = ime
    }

pauseCPU :: CPUEnvironment s -> ST s FrozenCPUEnvironment
pauseCPU cpu = do
    f_rom00   <- freeze $ rom00 cpu
    f_rom01   <- freeze $ rom01 cpu
    f_vram    <- freeze $ vram cpu
    f_extram  <- freeze $ extram cpu
    f_wram0   <- freeze $ wram0 cpu
    f_wram1   <- freeze $ wram1 cpu
    f_oam     <- freeze $ oam cpu
    f_ioports <- freeze $ ioports cpu
    f_hram    <- freeze $ hram cpu
    f_iereg   <- freeze $ iereg cpu
    f_a  <- readSTRef $ a cpu
    f_f  <- readSTRef $ f cpu
    f_b  <- readSTRef $ b cpu
    f_c  <- readSTRef $ c cpu
    f_d  <- readSTRef $ d cpu
    f_e  <- readSTRef $ e cpu
    f_h  <- readSTRef $ h cpu
    f_l  <- readSTRef $ l cpu
    f_sp <- readSTRef $ sp cpu
    f_pc <- readSTRef $ pc cpu
    f_ime <- readSTRef $ ime cpu
    return FrozenCPUEnvironment { 
        frz_a = f_a, frz_f = f_f, frz_b = f_b, frz_c = f_c
      , frz_d = f_d, frz_e = f_e, frz_h = f_h, frz_l = f_l
      , frz_sp = f_sp, frz_pc = f_pc 
      , frz_rom00 = f_rom00, frz_rom01 = f_rom01, frz_vram = f_vram, frz_extram = f_extram
      , frz_wram0 = f_wram0, frz_wram1 = f_wram1, frz_oam = f_oam, frz_ioports = f_ioports, frz_hram = f_hram
      , frz_iereg = f_iereg
      , frz_ime = f_ime
    }

-- Each pair of 8-bit registers can be accessed 
-- as a single 16-bit register.
data ComboRegister = AF | BC | DE | HL

registerPair :: ComboRegister -> (CPURegister s Word8, CPURegister s Word8)  
registerPair AF = (a, f)
registerPair BC = (b, c)
registerPair DE = (d, e)
registerPair HL = (h, l)

