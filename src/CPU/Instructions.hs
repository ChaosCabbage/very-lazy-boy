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
import qualified Data.Bits as Bit
import Data.Word    
import Control.Conditional (ifM)

-- Instructions can have either 0, 1 or 2 bytes as arguments.
data Instruction s = Ary0 (CPU s Cycles)
                   | Ary1 (Word8 -> CPU s Cycles)
                   | Ary2 (Word16 -> CPU s Cycles)
                   | Unimplemented -- Handy to have until I've finished

data Operation s = Op { 
    label :: String, 
    instruction :: Instruction s 
}

opTable :: Opcode -> Operation s
opTable opcode = case opcode of
    0x00 -> Op "NOP"            $ Ary0 $ nop
    0x10 -> Op "STOP 0"         $ Unimplemented
    0x20 -> Op "JR NZ,0x%02X"   $ Ary1 $ jrIf NZc
    0x30 -> Op "JR NC,0x%02X"   $ Ary1 $ jrIf NCc
    0x40 -> Op "LD B,B"         $ Unimplemented
    0x50 -> Op "LD D,B"         $ Unimplemented
    0x60 -> Op "LD H,B"         $ Unimplemented
    0x70 -> Op "LD (HL),B"      $ Unimplemented
    0x80 -> Op "ADD A,B"        $ Unimplemented
    0x90 -> Op "SUB B"          $ Unimplemented
    0xA0 -> Op "AND B"          $ Unimplemented
    0xB0 -> Op "OR B"           $ Unimplemented
    0xC0 -> Op "RET NZ"         $ Unimplemented
    0xD0 -> Op "RET NC"         $ Unimplemented
    0xE0 -> Op "LDH (0x%02X),A" $ Ary1 $ ldh_a8_reg a
    0xF0 -> Op "LDH A,(0x%02X)" $ Ary1 $ ldh_reg_a8 a    
    0x01 -> Op "LD BC,0x%04X"   $ Ary2 $ ld_reg_d16 BC
    0x11 -> Op "LD DE,0x%04X"   $ Ary2 $ ld_reg_d16 DE
    0x21 -> Op "LD HL,0x%04X"   $ Ary2 $ ld_reg_d16 HL
    0x31 -> Op "LD SP,0x%04X"   $ Ary2 $ ld_SP_d16
    0x41 -> Op "LD B,C"         $ Unimplemented
    0x51 -> Op "LD D,C"         $ Unimplemented
    0x61 -> Op "LD H,C"         $ Unimplemented
    0x71 -> Op "LD (HL),C"      $ Unimplemented
    0x81 -> Op "ADD A,C"        $ Unimplemented
    0x91 -> Op "SUB C"          $ Unimplemented
    0xA1 -> Op "AND C"          $ Unimplemented
    0xB1 -> Op "OR C"           $ Ary0 $ or_reg c    
    0xC1 -> Op "POP BC"         $ Unimplemented
    0xD1 -> Op "POP DE"         $ Unimplemented
    0xE1 -> Op "POP HL"         $ Unimplemented
    0xF1 -> Op "POP AF"         $ Unimplemented
    0x02 -> Op "LD (BC),A"      $ Unimplemented
    0x12 -> Op "LD (DE),A"      $ Unimplemented
    0x22 -> Op "LD (HL+),A"     $ Unimplemented
    0x32 -> Op "LD (HL-),A"     $ Ary0 $ ld_HLMinus_reg a
    0x42 -> Op "LD B,D"         $ Unimplemented
    0x52 -> Op "LD D,D"         $ Unimplemented
    0x62 -> Op "LD H,D"         $ Unimplemented
    0x72 -> Op "LD (HL),D"      $ Unimplemented
    0x82 -> Op "ADD A,D"        $ Unimplemented
    0x92 -> Op "SUB D"          $ Unimplemented
    0xA2 -> Op "AND D"          $ Unimplemented
    0xB2 -> Op "OR D"           $ Unimplemented
    0xC2 -> Op "JP NZ,0x%04X"   $ Unimplemented
    0xD2 -> Op "JP NC,0x%04X"   $ Unimplemented
    0xE2 -> Op "LD (C),A"       $ Ary0 $ ld_highC_A
    0xF2 -> Op "LD A,(C)"       $ Ary0 $ ld_A_highC     
    0x03 -> Op "INC BC"         $ Ary0 $ inc16 BC
    0x13 -> Op "INC DE"         $ Ary0 $ inc16 DE
    0x23 -> Op "INC HL"         $ Ary0 $ inc16 HL
    0x33 -> Op "INC SP"         $ Ary0 $ incSP
    0x43 -> Op "LD B,E"         $ Unimplemented
    0x53 -> Op "LD D,E"         $ Unimplemented
    0x63 -> Op "LD H,E"         $ Unimplemented
    0x73 -> Op "LD (HL),E"      $ Unimplemented
    0x83 -> Op "ADD A,E"        $ Unimplemented
    0x93 -> Op "SUB E"          $ Unimplemented
    0xA3 -> Op "AND E"          $ Unimplemented
    0xB3 -> Op "OR E"           $ Unimplemented
    0xC3 -> Op "JP 0x%04X"      $ Ary2 $ jp
    0xD3 -> Op "NOT USED"       $ Unimplemented
    0xE3 -> Op "NOT USED"       $ Unimplemented
    0xF3 -> Op "DI"             $ Ary0 $ di
    0x04 -> Op "INC B"          $ Ary0 $ inc b
    0x14 -> Op "INC D"          $ Ary0 $ inc d
    0x24 -> Op "INC H"          $ Ary0 $ inc h
    0x34 -> Op "INC (HL)"       $ Ary0 $ incDeref HL
    0x44 -> Op "LD B,H"         $ Unimplemented
    0x54 -> Op "LD D,H"         $ Unimplemented
    0x64 -> Op "LD H,H"         $ Unimplemented
    0x74 -> Op "LD (HL),H"      $ Unimplemented
    0x84 -> Op "ADD A,H"        $ Unimplemented
    0x94 -> Op "SUB H"          $ Unimplemented
    0xA4 -> Op "AND H"          $ Unimplemented
    0xB4 -> Op "OR H"           $ Unimplemented
    0xC4 -> Op "CALL NZ,0x%04X" $ Unimplemented
    0xD4 -> Op "CALL NC,0x%04X" $ Unimplemented
    0xE4 -> Op "NOT USED"       $ Unimplemented
    0xF4 -> Op "NOT USED"       $ Unimplemented
    0x05 -> Op "DEC B"          $ Ary0 $ dec b
    0x15 -> Op "DEC D"          $ Ary0 $ dec d
    0x25 -> Op "DEC H"          $ Ary0 $ dec h
    0x35 -> Op "DEC HL"         $ Ary0 $ decDeref HL
    0x45 -> Op "LD B,L"         $ Unimplemented
    0x55 -> Op "LD D,L"         $ Unimplemented
    0x65 -> Op "LD H,L"         $ Unimplemented
    0x75 -> Op "LD (HL),L"      $ Unimplemented
    0x85 -> Op "ADD A,L"        $ Unimplemented
    0x95 -> Op "SUB L"          $ Unimplemented
    0xA5 -> Op "AND L"          $ Unimplemented
    0xB5 -> Op "OR L"           $ Unimplemented
    0xC5 -> Op "PUSH BC"        $ Unimplemented
    0xD5 -> Op "PUSH DE"        $ Unimplemented
    0xE5 -> Op "PUSH HL"        $ Unimplemented
    0xF5 -> Op "PUSH AF"        $ Unimplemented
    0x06 -> Op "LD B,0x%02X"    $ Ary1 $ ld_reg_d8 b
    0x16 -> Op "LD D,0x%02X"    $ Ary1 $ ld_reg_d8 d
    0x26 -> Op "LD H,0x%02X"    $ Ary1 $ ld_reg_d8 h
    0x36 -> Op "LD (HL),0x%02X" $ Ary1 $ ld_deref_d8 HL
    0x46 -> Op "LD B,(HL)"      $ Unimplemented
    0x56 -> Op "LD D,(HL)"      $ Unimplemented
    0x66 -> Op "LD H,(HL)"      $ Unimplemented
    0x76 -> Op "HALT"           $ Unimplemented
    0x86 -> Op "ADD A,(HL)"     $ Unimplemented
    0x96 -> Op "SUB (HL)"       $ Unimplemented
    0xA6 -> Op "AND (HL)"       $ Unimplemented
    0xB6 -> Op "OR (HL)"        $ Unimplemented
    0xC6 -> Op "ADD A,0x%02X"   $ Unimplemented
    0xD6 -> Op "SUB 0x%02X"     $ Unimplemented
    0xE6 -> Op "AND 0x%02X"     $ Unimplemented
    0xF6 -> Op "OR 0x%02X"      $ Unimplemented
    0x07 -> Op "RLCA"           $ Unimplemented
    0x17 -> Op "RLA"            $ Unimplemented
    0x27 -> Op "DAA"            $ Unimplemented
    0x37 -> Op "SCF"            $ Unimplemented
    0x47 -> Op "LD B,A"         $ Unimplemented
    0x57 -> Op "LD D,A"         $ Unimplemented
    0x67 -> Op "LD H,A"         $ Unimplemented
    0x77 -> Op "LD (HL),A"      $ Unimplemented
    0x87 -> Op "ADD A,A"        $ Unimplemented
    0x97 -> Op "SUB A"          $ Unimplemented
    0xA7 -> Op "AND A"          $ Unimplemented
    0xB7 -> Op "OR A"           $ Unimplemented
    0xC7 -> Op "RST 00H"        $ Unimplemented
    0xD7 -> Op "RST 10H"        $ Unimplemented
    0xE7 -> Op "RST 20H"        $ Unimplemented
    0xF7 -> Op "RST 30H"        $ Unimplemented
    0x08 -> Op "LD (0x%04X),SP" $ Unimplemented
    0x18 -> Op "JR 0x%02X"      $ Ary1 $ jr
    0x28 -> Op "JR C,0x%02X"    $ Ary1 $ jrIf Zc
    0x38 -> Op "JR C,0x%02X"    $ Ary1 $ jrIf Cc
    0x48 -> Op "LD C,B"         $ Ary0 $ ld_reg_reg c b
    0x58 -> Op "LD E,B"         $ Ary0 $ ld_reg_reg e b
    0x68 -> Op "LD L,B"         $ Ary0 $ ld_reg_reg l b
    0x78 -> Op "LD A,B"         $ Ary0 $ ld_reg_reg a b
    0x88 -> Op "ADC A,B"        $ Unimplemented
    0x98 -> Op "SBC A,B"        $ Unimplemented
    0xA8 -> Op "XOR B"          $ Unimplemented
    0xB8 -> Op "CP B"           $ Unimplemented
    0xC8 -> Op "RET Z"          $ Unimplemented
    0xD8 -> Op "RET C"          $ Unimplemented
    0xE8 -> Op "ADD SP,0x%02X"  $ Unimplemented
    0xF8 -> Op "LD HL,SP+0x%02X"$ Unimplemented
    0x09 -> Op "ADD HL,BC"      $ Unimplemented
    0x19 -> Op "ADD HL,DE"      $ Unimplemented
    0x29 -> Op "ADD HL,HL"      $ Unimplemented 
    0x39 -> Op "ADD HL,SP"      $ Unimplemented
    0x49 -> Op "LD C,C"         $ Unimplemented
    0x59 -> Op "LD E,C"         $ Unimplemented
    0x69 -> Op "LD L,C"         $ Unimplemented
    0x79 -> Op "LD A,C"         $ Unimplemented
    0x89 -> Op "ADC A,C"        $ Unimplemented
    0x99 -> Op "SBC A,C"        $ Unimplemented
    0xA9 -> Op "XOR C"          $ Unimplemented
    0xB9 -> Op "CP C"           $ Unimplemented
    0xC9 -> Op "RET"            $ Ary0 $ ret
    0xD9 -> Op "RETI"           $ Ary0 $ reti
    0xE9 -> Op "JP (HL)"        $ Unimplemented
    0xF9 -> Op "LD SP,HL"       $ Unimplemented
    0x0A -> Op "LD A,(BC)"      $ Unimplemented
    0x1A -> Op "LD A,(DE)"      $ Unimplemented
    0x2A -> Op "LD A,(HL+)"     $ Ary0 $ ld_A_HLPlus
    0x3A -> Op "LD A,(HL-)"     $ Unimplemented
    0x4A -> Op "LD C,D"         $ Unimplemented
    0x5A -> Op "LD E,D"         $ Unimplemented
    0x6A -> Op "LD L,D"         $ Unimplemented
    0x7A -> Op "LD A,D"         $ Unimplemented
    0x8A -> Op "ADC A,D"        $ Unimplemented
    0x9A -> Op "SBC A,D"        $ Unimplemented
    0xAA -> Op "XOR D"          $ Unimplemented
    0xBA -> Op "CP D"           $ Unimplemented
    0xCA -> Op "JP Z,0x%04X"    $ Unimplemented
    0xDA -> Op "JP C,0x%04X"    $ Unimplemented
    0xEA -> Op "LD (0x%04X),A"  $ Ary2 $ ld_a16_reg a
    0xFA -> Op "LD A,(0x%04X)"  $ Unimplemented
    0x0B -> Op "DEC BC"         $ Ary0 $ dec16 BC
    0x1B -> Op "DEC DE"         $ Ary0 $ dec16 DE
    0x2B -> Op "DEC HL"         $ Ary0 $ dec16 HL
    0x3B -> Op "DEC SP"         $ Ary0 $ decSP
    0x4B -> Op "LD C,E"         $ Unimplemented
    0x5B -> Op "LD E,E"         $ Unimplemented
    0x6B -> Op "LD L,E"         $ Unimplemented
    0x7B -> Op "LD A,E"         $ Unimplemented
    0x8B -> Op "ADC A,E"        $ Unimplemented
    0x9B -> Op "SBC A,E"        $ Unimplemented
    0xAB -> Op "XOR E"          $ Unimplemented
    0xBB -> Op "CP E"           $ Unimplemented
    0xCB -> Op "PREFIX CB"      $ Unimplemented
    0xDB -> Op "NOT USED"       $ Unimplemented
    0xEB -> Op "NOT USED"       $ Unimplemented
    0xFB -> Op "EI"             $ Ary0 $ ei
    0x0C -> Op "INC C"          $ Ary0 $ inc c
    0x1C -> Op "INC E"          $ Unimplemented
    0x2C -> Op "INC L"          $ Unimplemented
    0x3C -> Op "INC A"          $ Unimplemented
    0x4C -> Op "LD C,H"         $ Unimplemented
    0x5C -> Op "LD E,H"         $ Unimplemented
    0x6C -> Op "LD L,H"         $ Unimplemented
    0x7C -> Op "LD A,H"         $ Unimplemented
    0x8C -> Op "ADC A,H"        $ Unimplemented
    0x9C -> Op "SBC A,H"        $ Unimplemented
    0xAC -> Op "XOR H"          $ Unimplemented
    0xBC -> Op "CP H"           $ Unimplemented
    0xCC -> Op "CALL Z,0x%04X"  $ Ary2 $ callIf Zc
    0xDC -> Op "CALL C,0x%04X"  $ Ary2 $ callIf Cc
    0xEC -> Op "NOT USED"       $ Unimplemented
    0xFC -> Op "NOT USED"       $ Unimplemented
    0x0D -> Op "DEC C"          $ Ary0 $ dec c
    0x1D -> Op "DEC E"          $ Unimplemented
    0x2D -> Op "DEC L"          $ Unimplemented
    0x3D -> Op "DEC A"          $ Unimplemented
    0x4D -> Op "LD C,L"         $ Unimplemented
    0x5D -> Op "LD E,L"         $ Unimplemented
    0x6D -> Op "LD L,L"         $ Unimplemented
    0x7D -> Op "LD A,L"         $ Unimplemented
    0x8D -> Op "ADC A,L"        $ Unimplemented
    0x9D -> Op "SBC A,L"        $ Unimplemented
    0xAD -> Op "XOR L"          $ Unimplemented
    0xBD -> Op "CP L"           $ Unimplemented
    0xCD -> Op "CALL 0x%04X"    $ Ary2 $ call
    0xDD -> Op "NOT USED"       $ Unimplemented
    0xED -> Op "NOT USED"       $ Unimplemented
    0xFD -> Op "NOT USED"       $ Unimplemented
    0x0E -> Op "LD C,0x%02X"    $ Ary1 $ ld_reg_d8 c
    0x1E -> Op "LD E,0x%02X"    $ Unimplemented
    0x2E -> Op "LD L,0x%02X"    $ Unimplemented
    0x3E -> Op "LD A,0x%02X"    $ Ary1 $ ld_reg_d8 a
    0x4E -> Op "LD C,(HL)"      $ Unimplemented
    0x5E -> Op "LD E,(HL)"      $ Unimplemented
    0x6E -> Op "LD L,(HL)"      $ Unimplemented
    0x7E -> Op "LD A,(HL)"      $ Unimplemented
    0x8E -> Op "ADC A,(HL)"     $ Unimplemented
    0x9E -> Op "SBC A,(HL)"     $ Unimplemented
    0xAE -> Op "XOR (HL)"       $ Unimplemented
    0xBE -> Op "CP (HL)"        $ Unimplemented
    0xCE -> Op "ADC A,0x%02X"   $ Unimplemented
    0xDE -> Op "SBC A,0x%02X"   $ Unimplemented
    0xEE -> Op "XOR 0x%02X"     $ Unimplemented
    0xFE -> Op "CP 0x%02X"      $ Ary1 $ cp
    0x0F -> Op "RRCA"           $ Unimplemented
    0x1F -> Op "RRA"            $ Unimplemented
    0x2F -> Op "CPL"            $ Ary0 $ cpl
    0x3F -> Op "CCF"            $ Unimplemented
    0x4F -> Op "LD C,A"         $ Unimplemented
    0x5F -> Op "LD E,A"         $ Unimplemented
    0x6F -> Op "LD L,A"         $ Unimplemented
    0x7F -> Op "LD A,A"         $ Unimplemented
    0x8F -> Op "ADC A,A"        $ Unimplemented
    0x9F -> Op "SBC A,A"        $ Unimplemented
    0xAF -> Op "XOR A"          $ Ary0 $ xor_reg a
    0xBF -> Op "CP A"           $ Unimplemented
    0xCF -> Op "RST 08H"        $ Unimplemented
    0xDF -> Op "RST 18H"        $ Unimplemented
    0xEF -> Op "RST 28H"        $ Unimplemented
    0xFF -> Op "RST 38H"        $ Unimplemented
    _    -> Op "???" Unimplemented

execute :: Instruction s -> CPU s Cycles
execute (Ary0 op) = op
execute (Ary1 op) = fetch >>= op
execute (Ary2 op) = fetch16 >>= op
execute Unimplemented = error "Trying to execute unimplemented opcode"

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

xor_reg :: CPURegister s Word8 -> CPU s Cycles
xor_reg reg = 
    xor =<< readReg reg

-- CPL: Complement the bits of A
-- Sets flags: - 1 1 -
cpl :: CPU s Cycles
cpl = 
    modifyReg a (Bit.complement) >>
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
    
-- EI: Enable interrupts!
ei :: CPU s Cycles
ei = 
    enableMasterInterrupt >>
    return 4
