module CPU.FrozenEnvironment (
    FrozenCPUEnvironment(..)
  , defaultCPU
  , readFrzMemory
    ) where

import CPU.Types
import Data.Word
import Data.Array
import ShowHex (showHex)

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
  -- Master interrupt flag
  , frz_ime :: Bool
} deriving (Show)

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
  , frz_a  = 0x01
  , frz_f  = 0xB0
  , frz_b  = 0x00
  , frz_c  = 0x13
  , frz_d  = 0x00
  , frz_e  = 0xD8
  , frz_h  = 0x01
  , frz_l  = 0x4D
  , frz_sp = 0xFFFE
  , frz_pc = 0x100
  , frz_ime = True
} 

-- The memory map is divided into different banks. 
-- Get the bank that this address points to.
-- Some banks are switchable - these are not yet dealt with.
memoryBank :: Address -> (FrozenCPUEnvironment -> Array Address Word8)
memoryBank addr
    | addr < 0x4000 = frz_rom00   -- 16KB Fixed cartridge rom bank.
    | addr < 0x8000 = frz_rom01   -- 16KB Switchable cartridge rom bank.
    | addr < 0xA000 = frz_vram    -- 8KB Video RAM.
    | addr < 0xC000 = frz_extram  -- 8KB Switchable RAM in cartridge.
    | addr < 0xD000 = frz_wram0   -- 4KB Work RAM.
    | addr < 0xE000 = frz_wram1   -- 4KB Work RAM.
    | addr < 0xFE00 = error $ "Memory access at " ++ (showHex addr) ++ ". " ++
                              "This is an 'echo' address. Not implemented yet :("
    | addr < 0xFEA0 = frz_oam     -- Sprite Attribute Table
    | addr < 0xFF00 = error $ "Memory access at unusable address: " ++ (showHex addr)
    | addr < 0xFF80 = frz_ioports -- IO ports
    | addr < 0xFFFF = frz_hram    -- 127 byte High RAM
    | addr == 0xFFFF = frz_iereg  -- Interrupt Enable register.


readFrzMemory :: Address -> FrozenCPUEnvironment -> Word8
readFrzMemory addr env = 
    (memoryBank addr) env ! addr

