module CPU.Environment (
    CPUEnvironment(..)
  , Register8(..)
  , Register16(..)
  , ComboRegister(..)
  , MemoryBank
  , IOPorts
  , resumeCPU
  , pauseCPU
  , register8
  , register16
  , registerPair
    ) where
    
import CPU.Types
import CPU.FrozenEnvironment
import qualified CPU.IORegisters as GBIO

import Data.Word
import Data.STRef
import Data.Array.ST
import Control.Monad.ST as ST

type CPURegister s w = (CPUEnvironment s -> STRef s w)
type MemoryBank s = CPUEnvironment s -> STUArray s Address Word8
type IOPorts s = CPUEnvironment s -> GBIO.IORegisters s

-- The whole state of the CPU and all the memory.
-- Mutable, for use in the ST monad.
data CPUEnvironment s = CPUEnvironment {
    -- Registers:
    a :: STRef s Word8
  , f :: STRef s Word8
  , b :: STRef s Word8
  , c :: STRef s Word8
  , d :: STRef s Word8
  , e :: STRef s Word8
  , h :: STRef s Word8
  , l :: STRef s Word8
  , sp :: STRef s Word16
  , pc :: STRef s Word16
  -- Memory banks:
  , rom00 :: STUArray s Address Word8
  , rom01 :: STUArray s Address Word8
  , vram :: STUArray s Address Word8
  , extram :: STUArray s Address Word8
  , wram0 :: STUArray s Address Word8
  , wram1 :: STUArray s Address Word8
  , oam :: STUArray s Address Word8
  , ioports :: GBIO.IORegisters s
  , hram :: STUArray s Address Word8
  , iereg :: STUArray s Address Word8
  -- Master interrupt flag
  , ime :: STRef s Bool
}

resumeCPU :: FrozenCPUEnvironment -> ST s (CPUEnvironment s)
resumeCPU state = do
    rom00   <- thaw $ frz_rom00 state
    rom01   <- thaw $ frz_rom01 state
    vram    <- thaw $ frz_vram state
    extram  <- thaw $ frz_extram state
    wram0   <- thaw $ frz_wram0 state
    wram1   <- thaw $ frz_wram1 state
    oam     <- thaw $ frz_oam state
    ioports <- GBIO.thaw $ frz_ioports state
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
    f_ioports <- GBIO.freeze $ ioports cpu
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

data Register8 = A | F | B | C | D | E | H | L

register8 :: Register8 -> (CPURegister s Word8)
register8 A = a
register8 F = f
register8 B = b
register8 C = c
register8 D = d
register8 E = e
register8 H = h
register8 L = l

data Register16 = SP | PC

register16 :: Register16 -> (CPURegister s Word16)
register16 SP = sp
register16 PC = pc

-- Each pair of 8-bit registers can be accessed 
-- as a single 16-bit register.
data ComboRegister = AF | BC | DE | HL

registerPair :: ComboRegister -> (Register8, Register8)  
registerPair AF = (A, F)
registerPair BC = (B, C)
registerPair DE = (D, E)
registerPair HL = (H, L)

