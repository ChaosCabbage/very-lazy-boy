module CPU.FrozenEnvironment (
    FrozenCPUEnvironment(..)
  , defaultCPU
    ) where

import CPU.Types
import Data.Word
import Data.Array

zeroMemory :: (Word16,Word16) -> Array Word16 Word8
zeroMemory (minIndex, maxIndex) =
    array (minIndex, maxIndex) [(i, 0) | i <- [minIndex..maxIndex]]

data FrozenCPUEnvironment = FrozenCPUEnvironment {
    -- Registers:
    frz_a :: Register8
  , frz_f :: Register8
  , frz_b :: Register8
  , frz_c :: Register8
  , frz_d :: Register8
  , frz_e :: Register8
  , frz_h :: Register8
  , frz_l :: Register8
  , frz_sp :: Register16
  , frz_pc :: Register16
  -- Memory banks:
  , frz_rom00 :: Array Address Word8
  , frz_rom01 :: Array Address Word8
  , frz_vram :: Array Address Word8
  , frz_extram :: Array Address Word8
  , frz_wram0 :: Array Address Word8
  , frz_wram1 :: Array Address Word8
  , frz_oam :: Array Address Word8
  , frz_ioports :: Array Address Word8
  , frz_hram :: Array Address Word8
  , frz_iereg :: Array Address Word8
}

defaultCPU :: FrozenCPUEnvironment
defaultCPU = FrozenCPUEnvironment {
    frz_rom00   = zeroMemory (0x0000, 0x3FFF) 
  , frz_rom01   = zeroMemory (0x4000, 0x7FFF)
  , frz_vram    = zeroMemory (0x8000, 0x9FFF)
  , frz_extram  = zeroMemory (0xA000, 0xBFFF)
  , frz_wram0   = zeroMemory (0xC000, 0xCFFF)
  , frz_wram1   = zeroMemory (0xD000, 0xDFFF)
  , frz_oam     = zeroMemory (0xFE00, 0xFE9F)
  , frz_ioports = zeroMemory (0xFF00, 0xFF7F)
  , frz_hram    = zeroMemory (0xFF80, 0xFFFE)
  , frz_iereg   = zeroMemory (0xFFFF, 0xFFFF)
  , frz_a  = 0x00
  , frz_f  = 0x00
  , frz_b  = 0x00
  , frz_c  = 0x00
  , frz_d  = 0x00
  , frz_e  = 0x00
  , frz_h  = 0x00
  , frz_l  = 0x00
  , frz_sp = 0x00
  , frz_pc = 0x100
}
