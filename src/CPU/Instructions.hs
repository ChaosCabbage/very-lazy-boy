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
-- execute 0x0E = LD C,d8  - 8 cycles
execute 0x21 = ld (directToCombo HL) =<< fetch16
execute 0xAF = xor =<< readReg a
execute 0xC3 = jp  =<< fetch16

execute x = error $ "Unknown opcode " ++ (showHex x)

-- Functions for different destinations
directToCombo :: ComboRegister -> (Word16 -> CPU s Cycles)
directToCombo reg = \w ->
    writeComboReg reg w >> 
    return 12 


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
ld :: (Word16 -> CPU s Cycles) -> Word16 -> CPU s Cycles
ld = id