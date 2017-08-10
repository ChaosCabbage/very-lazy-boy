module CPU.Instructions (
    Instruction(..)
  , label
  , execute
  , instruction
  , opTable
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
import Control.Conditional (ifM)

-- Instructions can have either 0, 1 or 2 bytes as arguments.
data Instruction s = Ary0 (CPU s Cycles)
                   | Ary1 (Word8 -> CPU s Cycles)
                   | Ary2 (Word16 -> CPU s Cycles)

data Operation s = Op { 
    label :: String, 
    instruction :: Instruction s 
}

opTable :: Opcode -> Operation s
opTable opcode = case opcode of
    0x00 -> Op "NOP"            $ Ary0 $ nop
--  0x10 -> Op "STOP"
    0x20 -> Op "JR NZ,0x%02X"   $ Ary1 $ jrIf NZc
    0x30 -> Op "JR NC,0x%02X"   $ Ary1 $ jrIf NCc
--  ...
    0xE0 -> Op "LDH (0x%02X),A" $ Ary1 $ ldh_a8_reg a
    0xF0 -> Op "LDH A,(0x%02X)" $ Ary1 $ ldh_reg_a8 a

    0x01 -> Op "LD BC,0x%04X"   $ Ary2 $ ld_reg_d16 BC
    0x11 -> Op "LD DE,0x%04X"   $ Ary2 $ ld_reg_d16 DE
    0x21 -> Op "LD HL,0x%04X"   $ Ary2 $ ld_reg_d16 HL
    0x31 -> Op "LD SP,0x%04X"   $ Ary2 $ ld_SP_d16
--  ...
    0xB1 -> Op "OR C"           $ Ary0 $ or_reg c

--  ...
    0x32 -> Op "LD (HL-),A"     $ Ary0 $ ld_HLMinus_reg a
--  ...
    0xE2 -> Op "LD (C),A"       $ Ary0 $ ld_highC_A
    0xF2 -> Op "LD A,(C)"       $ Ary0 $ ld_A_highC     

    0x03 -> Op "INC BC"         $ Ary0 $ inc16 BC
    0x13 -> Op "INC DE"         $ Ary0 $ inc16 DE
    0x23 -> Op "INC HL"         $ Ary0 $ inc16 HL
    0x33 -> Op "INC SP"         $ Ary0 $ incSP
--  ...
    0xC3 -> Op "JP 0x%04X"      $ Ary2 $ jp
--  0xD3    NOT USED
--  0xE3    NOT USED
    0xF3 -> Op "DI"             $ Ary0 $ di
    0x04 -> Op "INC B"          $ Ary0 $ inc b
    0x14 -> Op "INC D"          $ Ary0 $ inc d
    0x24 -> Op "INC H"          $ Ary0 $ inc h
    0x34 -> Op "INC (HL)"       $ Ary0 $ incDeref HL

    0x05 -> Op "DEC B"          $ Ary0 $ dec b
    0x15 -> Op "DEC D"          $ Ary0 $ dec d
    0x25 -> Op "DEC H"          $ Ary0 $ dec h
    0x35 -> Op "DEC HL"         $ Ary0 $ decDeref HL

    0x06 -> Op "LD B,0x%02X"    $ Ary1 $ ld_reg_d8 b
    0x16 -> Op "LD D,0x%02X"    $ Ary1 $ ld_reg_d8 d
    0x26 -> Op "LD H,0x%02X"    $ Ary1 $ ld_reg_d8 h
    0x36 -> Op "LD (HL),0x%02X" $ Ary1 $ ld_deref_d8 HL
    
    0x18 -> Op "JR 0x%02X"      $ Ary1 $ jr
    0x28 -> Op "JR C,0x%02X"    $ Ary1 $ jrIf Zc
    0x38 -> Op "JR C,0x%02X"    $ Ary1 $ jrIf Cc
    0x48 -> Op "LD C,B"         $ Ary0 $ ld_reg_reg c b
    0x58 -> Op "LD E,B"         $ Ary0 $ ld_reg_reg e b
    0x68 -> Op "LD L,B"         $ Ary0 $ ld_reg_reg l b
    0x78 -> Op "LD A,B"         $ Ary0 $ ld_reg_reg a b

    0xC9 -> Op "RET"            $ Ary0 $ ret
    0xD9 -> Op "RETI"           $ Ary0 $ reti

    0xEA -> Op "LD (0x%04X),A"  $ Ary2 $ ld_a16_reg a
    0x2A -> Op "LD A,(HL+)"     $ Ary0 $ ld_A_HLPlus
    0x0B -> Op "DEC BC"         $ Ary0 $ dec16 BC
    0x1B -> Op "DEC DE"         $ Ary0 $ dec16 DE
    0x2B -> Op "DEC HL"         $ Ary0 $ dec16 HL
    0x3B -> Op "DEC SP"         $ Ary0 $ decSP
    0x0C -> Op "INC C"          $ Ary0 $ inc c
    0xCC -> Op "CALL Z,0x%04X"  $ Ary2 $ callIf Zc
    0xDC -> Op "CALL C,0x%04X"  $ Ary2 $ callIf Cc
    0x0D -> Op "DEC C"          $ Ary0 $ dec c
    0xCD -> Op "CALL 0x%04X"    $ Ary2 $ call
    0x0E -> Op "LD C,0x%02X"    $ Ary1 $ ld_reg_d8 c
    0x3E -> Op "LD A,0x%02X"    $ Ary1 $ ld_reg_d8 a
    0xFE -> Op "CP 0x%02X"      $ Ary1 $ cp
    0xAF -> Op "XOR A"          $ Ary0 $ xor =<< readReg a
    _    -> error $ "Unknown opcode " ++ (showHex opcode)

execute :: Instruction s -> CPU s Cycles
execute (Ary0 op) = op
execute (Ary1 op) = fetch >>= op
execute (Ary2 op) = fetch16 >>= op

-- Flag conditions, for instructions such as "JP NZ,A"

data FlagCondition = Cc | NCc | Zc | NZc | Any

condition :: FlagCondition -> CPU s Bool
condition fc = case fc of
    Cc  -> readFlag C
    NCc -> fmap not (readFlag C)
    Zc  -> readFlag Z
    NZc -> fmap not (readFlag Z)
    Any -> return True

-- Functions for loading to different destinations

-- Write the contents of a register into a memory address.
-- The pointer is monadic, because in some cases the pointer
-- is incremented or decremented at the same time.
writeRegToPointer :: CPURegister s Word8 -> CPU s Address -> CPU s Cycles
writeRegToPointer reg pointer = do
    addr <- pointer
    readReg reg >>= writeMemory addr
    return 8 

-- Write the contents of a memory address into a register
-- See comment above about the monadic address.
writePointerToReg :: CPURegister s Word8 -> CPU s Address -> CPU s Cycles
writePointerToReg reg pointer =
    pointer >>= readMemory >>= writeReg reg >> 
    return 8

ld_reg_d16 :: ComboRegister -> Word16 -> CPU s Cycles
ld_reg_d16 reg d16 =
    writeComboReg reg d16 >> 
    return 12 

ld_SP_d16 :: Word16 -> CPU s Cycles
ld_SP_d16 d16 =
    writeReg sp d16 >> 
    return 12

ld_HLMinus_reg :: CPURegister s Word8 -> CPU s Cycles
ld_HLMinus_reg reg = 
    writeRegToPointer reg (pointerMinus HL)

ld_HLPlus_reg :: CPURegister s Word8 -> CPU s Cycles
ld_HLPlus_reg reg =
    writeRegToPointer reg (pointerPlus HL)

-- For e.g. LD (HL-) instructions.
-- Get the address pointed to by the register, then decrement the register.
pointerMinus :: ComboRegister -> CPU s Address
pointerMinus reg = do
    addr <- readComboReg reg
    modifyComboReg reg (subtract 1)
    return addr

-- e.g. LD (HL+) instructions
pointerPlus :: ComboRegister -> CPU s Address
pointerPlus reg = do
    addr <- readComboReg reg
    modifyComboReg reg (+ 1)
    return addr

derefWrite :: ComboRegister -> Word8 -> CPU s ()
derefWrite reg d8 = do
    addr <- readComboReg reg 
    writeMemory addr d8 

deref :: ComboRegister -> CPU s Word8
deref reg =
    readComboReg reg >>= readMemory 

derefModify :: ComboRegister -> (Word8 -> Word8) -> CPU s ()
derefModify reg modifier =
    (modifier <$> deref reg) >>= derefWrite reg

-- Several instructions add 0xFF00 to their argument
-- to get an address.
highAddress :: Word8 -> Address
highAddress w8 = 
    0xFF00 + (fromIntegral w8 :: Word16)


-- NOP: Blissfully let life pass you by.
nop :: CPU s Cycles
nop = return 4

-- JP: Unconditional jump to an address.
jp :: Address -> CPU s Cycles
jp addr = 
    writeReg pc addr >> 
    return 16

-- OR: Or the contents of A with the argument
-- Flags: Z 0 0 0
or_d8 :: Word8 -> CPU s Cycles
or_d8 d8 = do
    modifyReg a (Bit..|. d8)
    va <- readReg a
    setFlags (As (va == 0), Off, Off, Off)
    return 8

or_reg :: CPURegister s Word8 -> CPU s Cycles
or_reg reg = 
    readReg reg >>= or_d8 >> 
    return 4

-- XOR: Exclusive-or the contents of A with the argument.
-- Sets flags: Z 0 0 0
xor :: Word8 -> CPU s Cycles
xor byte = do
    modifyReg a (Bit.xor byte) 
    -- Set the flags.
    va <- readReg a
    setFlags (As (va == 0), Off, Off, Off)
    return 4

-- LD: Load bytes into a destination.
--
-- Lots of variations of this.
-- Some are genuine variations, some are because of my data structures. 
-- (particularly the fact that a ComboRegister is a different type 
--  from the actual 16 bit registers)

ld_reg_d8 :: CPURegister s Word8 -> Word8 -> CPU s Cycles
ld_reg_d8 reg d8 = do
    writeReg reg d8 
    return 8

ld_reg_reg :: CPURegister s Word8 -> CPURegister s Word8 -> CPU s Cycles
ld_reg_reg to from = do
    readReg from >>= writeReg to
    return 4

ld_deref_d8 :: ComboRegister -> Word8 -> CPU s Cycles
ld_deref_d8 reg w8 = do
    derefWrite reg w8
    return 12

-- LDH (a8),reg: 
-- Load the contents of register into address (a8 + 0xFF00)
ldh_a8_reg :: CPURegister s Word8 -> Word8 -> CPU s Cycles
ldh_a8_reg reg a8 = do
    let addr = highAddress a8
    readReg reg >>= writeMemory addr
    return 12

-- LDH register,(a8): 
-- Load the contents of (a8 + 0xFF00) into register
ldh_reg_a8 :: CPURegister s Word8 -> Word8 -> CPU s Cycles
ldh_reg_a8 reg a8 = do
    let addr = highAddress a8
    readMemory addr >>= writeReg reg
    return 12

-- LD (a16),reg:
-- Load the contents of reg into the address (a16)
ld_a16_reg :: CPURegister s Word8 -> Word16 -> CPU s Cycles
ld_a16_reg reg a16 = do
    readReg reg >>= writeMemory a16
    return 16

-- LD A,(HL+)
-- Dereference HL, write the value to A, and then increment HL
ld_A_HLPlus :: CPU s Cycles
ld_A_HLPlus = 
    writePointerToReg a (pointerPlus HL)

-- LD (C),A
ld_highC_A :: CPU s Cycles
ld_highC_A = do
    addr <- highAddress <$> (readReg c)
    readReg a >>= writeMemory addr 
    return 8

-- LD A,(C)
ld_A_highC :: CPU s Cycles
ld_A_highC = do
    addr <- highAddress <$> (readReg c)
    readMemory addr >>= writeReg a
    return 8

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

dec16 :: ComboRegister -> CPU s Cycles
dec16 reg = do
    modifyComboReg reg (subtract 1) 
    return 8

decSP :: CPU s Cycles
decSP = do
    modifyReg sp (subtract 1)
    return 8

-- Flags Z 1 H -
decDeref :: ComboRegister -> CPU s Cycles
decDeref reg = do
    derefModify reg (subtract 1)
    v <- readComboReg reg
    setFlags (As (v == 0), On, Off, NA) -- half-carry is complicated, gonna ignore it right now
    return 12

-- Flags: Z 0 H -
inc :: CPURegister s Word8 -> CPU s Cycles
inc reg = do
    modifyReg reg (+ 1)
    v <- readReg reg
    setFlags (As (v == 0), Off, Off, NA) -- half-carry is complicated, gonna ignore it right now
    return 4

inc16 :: ComboRegister -> CPU s Cycles
inc16 reg = do
    modifyComboReg reg (+1)
    return 8

incSP :: CPU s Cycles
incSP = do
    modifyReg sp (+1)
    return 8

-- Flags Z 0 H -
incDeref :: ComboRegister -> CPU s Cycles
incDeref reg = do
    derefModify reg (+ 1)
    v <- readComboReg reg
    setFlags (As (v == 0), Off, Off, NA) -- half-carry is complicated, gonna ignore it right now
    return 12

-- CP: Compare with A to set flags.
-- Flags: Z 1 H C
cp :: Word8 -> CPU s Cycles
cp byte = do
    av <- readReg a
    let (result, cv) = carriedSubtract av byte 
    setFlags (As (result == 0), On, Off, As cv)
    return 8


-- JR: Relative jump
-- cc is a flag condition. 
-- e is a *signed* 8-bit number.

jr :: Word8 -> CPU s Cycles
jr d8 = 
    modifyReg pc (+ signed) >> return 12
    where signed = fromIntegral $ toSigned d8
--
-- If condition is met, PC := PC + e (12 cycles)
-- else                 continue     (8 cycles)
jrIf :: FlagCondition -> Word8 -> CPU s Cycles
jrIf cc d8 = 
    ifM (condition cc)
        (jr d8)
        (return 8)
            

call :: Word16 -> CPU s Cycles
call a16 = do
    readReg pc >>= pushOntoStack
    jumpTo a16
    return 24

callIf :: FlagCondition -> Word16 -> CPU s Cycles
callIf cc addr = 
    ifM (condition cc)
        (call addr)
        (return 12)

-- RETurn from a function call.
ret :: CPU s Cycles 
ret = do
    addr <- popFromStack
    jumpTo addr
    return 16

retIf :: FlagCondition -> CPU s Cycles
retIf cc = 
    ifM (condition cc)
        (ret >> return 20)
        (return 8)

-- Return and enable interrupts.
reti :: CPU s Cycles
reti = 
    enableMasterInterrupt >> ret
    

-- DI: Disable interrupts
di :: CPU s Cycles
di = 
    disableMasterInterrupt >> 
    return 4 
