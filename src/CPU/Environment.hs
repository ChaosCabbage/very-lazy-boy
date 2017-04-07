module CPU.Environment (
    CPUEnvironment(..)
  , ComboRegister(..)
  , MemoryBank
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

-- Each pair of 8-bit registers can be accessed 
-- as a single 16-bit register.
data ComboRegister = AF | BC | DE | HL

initCPU :: Rom -> ST s (CPUEnvironment s)
initCPU rom = do
    rom00 <- thaw rom
    rom01   <- newArray (0x4000,0x7FFF) 0x00
    vram    <- newArray (0x8000,0x9FFF) 0x00
    extram  <- newArray (0xA000,0xBFFF) 0x00
    wram0   <- newArray (0xC000,0xCFFF) 0x00
    wram1   <- newArray (0xD000,0xDFFF) 0x00
    oam     <- newArray (0xFE00,0xFE9F) 0x00
    ioports <- newArray (0xFF00,0xFF7F) 0x00
    hram    <- newArray (0xFF80,0xFFFE) 0x00
    iereg   <- newArray (0xFFFF,0xFFFF) 0x00
    a  <- newSTRef 0x00
    f  <- newSTRef 0x00
    b  <- newSTRef 0x00
    c  <- newSTRef 0x00
    d  <- newSTRef 0x00
    e  <- newSTRef 0x00
    h  <- newSTRef 0x00
    l  <- newSTRef 0x00
    sp <- newSTRef 0x00
    pc <- newSTRef 0x100
    return CPUEnvironment { 
        a = a
      , f = f
      , b = b
      , c = c
      , d = d
      , e = e
      , h = h
      , l = l
      , sp = sp
      , pc = pc 
      , rom00 = rom00
      , rom01 = rom01
      , vram = vram
      , extram = extram
      , wram0 = wram0
      , wram1 = wram1
      , oam = oam
      , ioports = ioports
      , hram = hram
      , iereg = iereg
    }

registerPair :: ComboRegister -> (CPURegister s Word8, CPURegister s Word8)  
registerPair AF = (a, f)
registerPair BC = (b, c)
registerPair DE = (d, e)
registerPair HL = (h, l)

