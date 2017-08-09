module CPU.Instructions (
    execute
    ) where

import BitTwiddling
import CPU.Types
import CPU.Environment
import CPU.Flags
import CPU.Arithmetic
import CPU
import ShowHex

import qualified Data.Bits as Bit
import Data.Word    

-- For reasons I haven't completely figured out yet,
-- the arguments to the combo register instructions are flipped.
-- This does NOT affect jump instructions.
-- TODO: Investigate gameboy endianness.


execute :: Opcode -> CPU s Cycles
execute op = case op of
    0x00 -> nop
    0x01 -> ld (directToCombo BC) =<< fetch16  -- LD BC,d16
    0x05 -> dec b                              -- DEC B
    0x06 -> ld (direct b) =<< fetch            -- LD B,d8
    0x0D -> dec c                              -- DEC C
    0x0E -> ld (direct c) =<< fetch            -- LD C,d8
    0x20 -> jr NZc =<< fetch                   -- JR NZ,r8
    0x21 -> ld (directToCombo HL) =<< fetch16  -- LD HL,d16
    0x31 -> ld (direct16 sp) =<< fetch16       -- LD SP,d16
    0x32 -> ld (derefMinus HL) =<< readReg a   -- LD (HL-),A 
    0x3E -> ld (direct a) =<< fetch            -- LD A,d8
    0x36 -> (ld (deref HL) =<< fetch) >> return 12    -- LD (HL),d8
    0xAF -> xor =<< readReg a                  -- XOR A
    0xC3 -> jp =<< fetch16                     -- JP d16
    0xE0 -> ldh_a8_a                           -- LDH (a8),A
    0xEA -> ld derefImmediate =<< readReg a    -- LD (a16),A 
    0xF0 -> ldh_a_a8                           -- LDH A,(a8)
    0xFE -> cp =<< fetch                       -- CP d8
    0xF3 -> di                                 -- DI
    _    -> error $ "Unknown opcode " ++ (showHex op)


-- Flag conditions, for instructions such as "JP NZ,A"

data FlagCondition = Cc | NCc | Zc | NZc | Any

condition :: FlagCondition -> CPU s Bool
condition f = case f of
    Cc  -> readFlag C
    NCc -> fmap not (readFlag C)
    Zc  -> readFlag Z
    NZc -> fmap not (readFlag Z)
    Any -> return True

-- Functions for loading to different destinations
direct :: CPURegister s Word8 -> Word8 -> CPU s Cycles
direct reg w =
    writeReg reg w >>
    return 8

direct16 :: CPURegister s Word16 -> (Word16 -> CPU s Cycles)
direct16 reg w = 
    writeReg reg w >>
    return 12

directToCombo :: ComboRegister -> (Word16 -> CPU s Cycles)
directToCombo reg = \w ->
    writeComboReg reg w >> 
    return 12 

-- e.g. LD (HL-) instructions.
-- Dereference the register, write the value to the address,
-- then decrement the register.
derefMinus :: ComboRegister -> Word8 -> CPU s Cycles
derefMinus reg w = do
    addr <- readComboReg reg
    writeMemory addr w
    writeComboReg reg (addr - 1)
    return 8

deref :: ComboRegister -> Word8 -> CPU s Cycles
deref reg w = do
    addr <- readComboReg reg
    writeMemory addr w
    return 8

derefImmediate :: Word8 -> CPU s Cycles
derefImmediate w = do
    addr <- fetch16
    writeMemory addr w
    return 16

-- NOP: Blissfully let life pass you by.
nop :: CPU s Cycles
nop = return 4

-- JP: Unconditional jump to an address.
jp :: Address -> CPU s Cycles
jp addr = 
    writeReg pc addr >> 
    return 16

-- XOR: Exclusive-or the contents of A with the argument.
-- Sets flags: Z 0 0 0
xor :: Word8 -> CPU s Cycles
xor byte = do
    modifyReg a (Bit.xor byte) 
    -- Set the flags.
    a <- readReg a
    setFlags (As (a == 0), Off, Off, Off)
    return 4

-- LD: Load bytes into a destination.
ld :: (w -> CPU s Cycles) -> w -> CPU s Cycles
ld = id

-- LDH (a8),A: 
-- Load the contents of A into address (a8 + 0xFF00)
-- Cheated a bit in defining this. It should take a destination function like the others.
ldh_a8_a :: CPU s Cycles
ldh_a8_a = do
    a8 <- fetch
    let addr = 0xFF00 + (fromIntegral a8 :: Word16)
    a <- readReg a
    writeMemory addr a
    return 12

-- LDH A,(a8): 
-- Load the contents of (a8 + 0xFF00) into A
-- Cheated a bit in defining this. It should take a destination function like the others.
ldh_a_a8 :: CPU s Cycles
ldh_a_a8 = do
    a8 <- fetch
    let addr = 0xFF00 + (fromIntegral a8 :: Word16)
    v <- readMemory addr
    writeReg a v
    return 12

-- DEC: Decrease a register or memory location by 1
-- Currently just the 8-bit registers.
-- Need to rethink this when I get to the others.
-- Flags: Z 1 H -
dec :: CPURegister s Word8 -> CPU s Cycles
dec reg = do
    modifyReg reg (subtract 1)
    v <- readReg reg
    setFlags (As (v == 0), On, Off, NA) -- half-carry is complicated, gonna ignore it right now
    return 4

-- CP: Compare with A to set flags.
-- Flags: Z 1 H C
cp :: Word8 -> CPU s Cycles
cp byte = do
    a <- readReg a
    let (result, c) = carriedSubtract a byte 
    setFlags (As (result == 0), On, Off, As c)
    return 8


-- JR: Relative conditional jump
-- cc is a flag condition. 
-- e is a *signed* 8-bit number.
--
-- If condition is met, PC := PC + e (12 cycles)
-- else                 continue     (8 cycles)
jr :: FlagCondition -> Word8 -> CPU s Cycles
jr cc byte = do
    jump <- condition cc
    if jump 
        then 
            modifyReg pc (+ signed) >>
            return 12                       
        else 
            return 8
            
    where signed = fromIntegral $ toSigned byte

-- DI: Disable interrupts
di :: CPU s Cycles
di = 
    disableMasterInterrupt >> 
    return 4 
