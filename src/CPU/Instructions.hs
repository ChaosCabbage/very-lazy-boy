module CPU.Instructions (
    execute
  , step
    ) where

import CPU.Types
import CPU.Environment
import CPU
import ShowHex

import qualified Data.Bits as Bit
import Data.Word


type Cycles = Int

step :: CPU s Cycles
step = fetch >>= execute

execute :: Opcode -> CPU s Cycles
execute 0x00 = nop
execute 0x01 = ld (directToCombo BC) =<< fetch16  -- LD BC,d16
execute 0x06 = ld (direct b) =<< fetch            -- LD B,d8
execute 0x0E = ld (direct c) =<< fetch            -- LD C,d8
execute 0x21 = ld (directToCombo HL) =<< fetch16  -- LD HL,d16
execute 0x32 =                                    -- LD (HL-),A 
execute 0xAF = xor =<< readReg a                  -- XOR A
execute 0xC3 = jp  =<< fetch16                    -- JP d16

execute x = error $ "Unknown opcode " ++ (showHex x)

-- Functions for loading to different destinations
direct :: CPURegister s Word8 -> (Word8 -> CPU s Cycles)
direct reg = \w ->
    writeReg reg w >>
    return 8

directToSP :: Word16 -> CPU s Cycles
directToSP w =
    writeReg sp w >>
    return 12

directToCombo :: ComboRegister -> (Word16 -> CPU s Cycles)
directToCombo reg = \w ->
    writeComboReg reg w >> 
    return 12 

-- e.g. LD (HL-) instructions.
-- Dereference the register, write the value to the address,
-- then decrement the register.
derefMinus :: ComboRegister -> (Word16 -> CPU s Cycles)
derefMinus reg = \w -> do
    addr <- readComboReg reg
    writeMemory addr w
    writeComboReg reg (addr - 1)
    return 8

-- NOP: Blissfully let life pass you by.
nop :: CPU s Cycles
nop = return 4

-- JP: Unconditional jump to an address.
jp :: Address -> CPU s Cycles
jp addr = 
    writeReg pc addr >> 
    return 16

-- XOR: Exclusive-or the contents of A with the argument.
xor :: Word8 -> CPU s Cycles
xor byte = do
    aVal <- readReg a
    writeReg a $ aVal `Bit.xor` byte
    return 4

-- LD: Load bytes into a destination.
ld :: (w -> CPU s Cycles) -> w -> CPU s Cycles
ld = id