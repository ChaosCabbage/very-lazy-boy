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
    0x00 -> Op "NOP"            $ nop
    0x10 -> Op "STOP 0"         $ Unimplemented
    0x20 -> Op "JR NZ,0x%02X"   $ jrIf NZc
    0x30 -> Op "JR NC,0x%02X"   $ jrIf NCc
    0x40 -> Op "LD B,B"         $ ld_reg_reg b b
    0x50 -> Op "LD D,B"         $ ld_reg_reg d b
    0x60 -> Op "LD H,B"         $ ld_reg_reg h b
    0x70 -> Op "LD (HL),B"      $ ld_deref_reg HL b
    0x80 -> Op "ADD A,B"        $ Unimplemented
    0x90 -> Op "SUB B"          $ Unimplemented
    0xA0 -> Op "AND B"          $ Unimplemented
    0xB0 -> Op "OR B"           $ Unimplemented
    0xC0 -> Op "RET NZ"         $ ret NZc
    0xD0 -> Op "RET NC"         $ ret NCc
    0xE0 -> Op "LDH (0x%02X),A" $ ldh_a8_reg a
    0xF0 -> Op "LDH A,(0x%02X)" $ ldh_reg_a8 a    
    0x01 -> Op "LD BC,0x%04X"   $ ld_reg_d16 BC
    0x11 -> Op "LD DE,0x%04X"   $ ld_reg_d16 DE
    0x21 -> Op "LD HL,0x%04X"   $ ld_reg_d16 HL
    0x31 -> Op "LD SP,0x%04X"   $ ld_SP_d16
    0x41 -> Op "LD B,C"         $ ld_reg_reg b c
    0x51 -> Op "LD D,C"         $ ld_reg_reg d c
    0x61 -> Op "LD H,C"         $ ld_reg_reg h c
    0x71 -> Op "LD (HL),C"      $ ld_deref_reg HL c
    0x81 -> Op "ADD A,C"        $ Unimplemented
    0x91 -> Op "SUB C"          $ Unimplemented
    0xA1 -> Op "AND C"          $ Unimplemented
    0xB1 -> Op "OR C"           $ or_reg c    
    0xC1 -> Op "POP BC"         $ pop BC
    0xD1 -> Op "POP DE"         $ pop DE
    0xE1 -> Op "POP HL"         $ pop HL
    0xF1 -> Op "POP AF"         $ pop AF
    0x02 -> Op "LD (BC),A"      $ ld_deref_reg BC a
    0x12 -> Op "LD (DE),A"      $ ld_deref_reg DE a
    0x22 -> Op "LD (HL+),A"     $ ld_HLPlus_reg a
    0x32 -> Op "LD (HL-),A"     $ ld_HLMinus_reg a
    0x42 -> Op "LD B,D"         $ ld_reg_reg b d
    0x52 -> Op "LD D,D"         $ ld_reg_reg d d
    0x62 -> Op "LD H,D"         $ ld_reg_reg h d
    0x72 -> Op "LD (HL),D"      $ ld_deref_reg HL d
    0x82 -> Op "ADD A,D"        $ Unimplemented
    0x92 -> Op "SUB D"          $ Unimplemented
    0xA2 -> Op "AND D"          $ Unimplemented
    0xB2 -> Op "OR D"           $ Unimplemented
    0xC2 -> Op "JP NZ,0x%04X"   $ jpIf NZc
    0xD2 -> Op "JP NC,0x%04X"   $ jpIf NCc
    0xE2 -> Op "LD (C),A"       $ ld_highC_A
    0xF2 -> Op "LD A,(C)"       $ ld_A_highC     
    0x03 -> Op "INC BC"         $ inc16 BC
    0x13 -> Op "INC DE"         $ inc16 DE
    0x23 -> Op "INC HL"         $ inc16 HL
    0x33 -> Op "INC SP"         $ incSP
    0x43 -> Op "LD B,E"         $ ld_reg_reg b e
    0x53 -> Op "LD D,E"         $ ld_reg_reg d e
    0x63 -> Op "LD H,E"         $ ld_reg_reg h e
    0x73 -> Op "LD (HL),E"      $ ld_deref_reg HL e
    0x83 -> Op "ADD A,E"        $ Unimplemented
    0x93 -> Op "SUB E"          $ Unimplemented
    0xA3 -> Op "AND E"          $ Unimplemented
    0xB3 -> Op "OR E"           $ or_reg e
    0xC3 -> Op "JP 0x%04X"      $ jp
    0xD3 -> Op "NOT USED"       $ Unimplemented
    0xE3 -> Op "NOT USED"       $ Unimplemented
    0xF3 -> Op "DI"             $ di
    0x04 -> Op "INC B"          $ inc b
    0x14 -> Op "INC D"          $ inc d
    0x24 -> Op "INC H"          $ inc h
    0x34 -> Op "INC (HL)"       $ incDeref HL
    0x44 -> Op "LD B,H"         $ ld_reg_reg b h
    0x54 -> Op "LD D,H"         $ ld_reg_reg d h
    0x64 -> Op "LD H,H"         $ ld_reg_reg h h
    0x74 -> Op "LD (HL),H"      $ Unimplemented
    0x84 -> Op "ADD A,H"        $ Unimplemented
    0x94 -> Op "SUB H"          $ Unimplemented
    0xA4 -> Op "AND H"          $ Unimplemented
    0xB4 -> Op "OR H"           $ or_reg h
    0xC4 -> Op "CALL NZ,0x%04X" $ callIf NZc
    0xD4 -> Op "CALL NC,0x%04X" $ callIf NCc
    0xE4 -> Op "NOT USED"       $ Unimplemented
    0xF4 -> Op "NOT USED"       $ Unimplemented
    0x05 -> Op "DEC B"          $ dec b
    0x15 -> Op "DEC D"          $ dec d
    0x25 -> Op "DEC H"          $ dec h
    0x35 -> Op "DEC (HL)"       $ decDeref HL
    0x45 -> Op "LD B,L"         $ ld_reg_reg b l
    0x55 -> Op "LD D,L"         $ ld_reg_reg d l
    0x65 -> Op "LD H,L"         $ ld_reg_reg h l
    0x75 -> Op "LD (HL),L"      $ ld_deref_reg HL l
    0x85 -> Op "ADD A,L"        $ Unimplemented
    0x95 -> Op "SUB L"          $ Unimplemented
    0xA5 -> Op "AND L"          $ Unimplemented
    0xB5 -> Op "OR L"           $ or_reg l
    0xC5 -> Op "PUSH BC"        $ push BC
    0xD5 -> Op "PUSH DE"        $ push DE
    0xE5 -> Op "PUSH HL"        $ push HL
    0xF5 -> Op "PUSH AF"        $ push AF
    0x06 -> Op "LD B,0x%02X"    $ ld_reg_d8 b
    0x16 -> Op "LD D,0x%02X"    $ ld_reg_d8 d
    0x26 -> Op "LD H,0x%02X"    $ ld_reg_d8 h
    0x36 -> Op "LD (HL),0x%02X" $ ld_deref_d8 HL
    0x46 -> Op "LD B,(HL)"      $ ld_reg_deref b HL
    0x56 -> Op "LD D,(HL)"      $ ld_reg_deref d HL
    0x66 -> Op "LD H,(HL)"      $ ld_reg_deref h HL
    0x76 -> Op "HALT"           $ Unimplemented
    0x86 -> Op "ADD A,(HL)"     $ Unimplemented
    0x96 -> Op "SUB (HL)"       $ Unimplemented
    0xA6 -> Op "AND (HL)"       $ Unimplemented
    0xB6 -> Op "OR (HL)"        $ Unimplemented
    0xC6 -> Op "ADD A,0x%02X"   $ Unimplemented
    0xD6 -> Op "SUB 0x%02X"     $ Unimplemented
    0xE6 -> Op "AND 0x%02X"     $ Unimplemented
    0xF6 -> Op "OR 0x%02X"      $ or_d8
    0x07 -> Op "RLCA"           $ Unimplemented
    0x17 -> Op "RLA"            $ Unimplemented
    0x27 -> Op "DAA"            $ Unimplemented
    0x37 -> Op "SCF"            $ Unimplemented
    0x47 -> Op "LD B,A"         $ ld_reg_reg b a
    0x57 -> Op "LD D,A"         $ ld_reg_reg d a
    0x67 -> Op "LD H,A"         $ ld_reg_reg h a
    0x77 -> Op "LD (HL),A"      $ ld_deref_reg HL a
    0x87 -> Op "ADD A,A"        $ Unimplemented
    0x97 -> Op "SUB A"          $ Unimplemented
    0xA7 -> Op "AND A"          $ Unimplemented
    0xB7 -> Op "OR A"           $ or_reg a
    0xC7 -> Op "RST 00H"        $ rst 0x00
    0xD7 -> Op "RST 10H"        $ rst 0x10
    0xE7 -> Op "RST 20H"        $ rst 0x20
    0xF7 -> Op "RST 30H"        $ rst 0x30
    0x08 -> Op "LD (0x%04X),SP" $ Unimplemented
    0x18 -> Op "JR 0x%02X"      $ jr
    0x28 -> Op "JR C,0x%02X"    $ jrIf Zc
    0x38 -> Op "JR C,0x%02X"    $ jrIf Cc
    0x48 -> Op "LD C,B"         $ ld_reg_reg c b
    0x58 -> Op "LD E,B"         $ ld_reg_reg e b
    0x68 -> Op "LD L,B"         $ ld_reg_reg l b
    0x78 -> Op "LD A,B"         $ ld_reg_reg a b
    0x88 -> Op "ADC A,B"        $ Unimplemented
    0x98 -> Op "SBC A,B"        $ Unimplemented
    0xA8 -> Op "XOR B"          $ xor_reg b
    0xB8 -> Op "CP B"           $ cp_reg b
    0xC8 -> Op "RET Z"          $ retIf Zc
    0xD8 -> Op "RET C"          $ retIf Cc
    0xE8 -> Op "ADD SP,0x%02X"  $ Unimplemented
    0xF8 -> Op "LD HL,SP+0x%02X"$ Unimplemented
    0x09 -> Op "ADD HL,BC"      $ Unimplemented
    0x19 -> Op "ADD HL,DE"      $ Unimplemented
    0x29 -> Op "ADD HL,HL"      $ Unimplemented 
    0x39 -> Op "ADD HL,SP"      $ Unimplemented
    0x49 -> Op "LD C,C"         $ ld_reg_reg c c
    0x59 -> Op "LD E,C"         $ ld_reg_reg e c
    0x69 -> Op "LD L,C"         $ ld_reg_reg l c
    0x79 -> Op "LD A,C"         $ ld_reg_reg a c
    0x89 -> Op "ADC A,C"        $ Unimplemented
    0x99 -> Op "SBC A,C"        $ Unimplemented
    0xA9 -> Op "XOR C"          $ xor_reg c
    0xB9 -> Op "CP C"           $ cp_reg c
    0xC9 -> Op "RET"            $ ret
    0xD9 -> Op "RETI"           $ reti
    0xE9 -> Op "JP (HL)"        $ Unimplemented
    0xF9 -> Op "LD SP,HL"       $ Unimplemented
    0x0A -> Op "LD A,(BC)"      $ ld_reg_deref a BC
    0x1A -> Op "LD A,(DE)"      $ ld_reg_deref a DE
    0x2A -> Op "LD A,(HL+)"     $ ld_A_HLPlus
    0x3A -> Op "LD A,(HL-)"     $ Unimplemented
    0x4A -> Op "LD C,D"         $ ld_reg_reg c d
    0x5A -> Op "LD E,D"         $ ld_reg_reg e d
    0x6A -> Op "LD L,D"         $ ld_reg_reg l d
    0x7A -> Op "LD A,D"         $ ld_reg_reg a d
    0x8A -> Op "ADC A,D"        $ Unimplemented
    0x9A -> Op "SBC A,D"        $ Unimplemented
    0xAA -> Op "XOR D"          $ xor_reg d
    0xBA -> Op "CP D"           $ cp_reg d
    0xCA -> Op "JP Z,0x%04X"    $ jpIf Zc
    0xDA -> Op "JP C,0x%04X"    $ jpIf Cc
    0xEA -> Op "LD (0x%04X),A"  $ ld_a16_reg a
    0xFA -> Op "LD A,(0x%04X)"  $ ld_reg_a16 a
    0x0B -> Op "DEC BC"         $ dec16 BC
    0x1B -> Op "DEC DE"         $ dec16 DE
    0x2B -> Op "DEC HL"         $ dec16 HL
    0x3B -> Op "DEC SP"         $ decSP
    0x4B -> Op "LD C,E"         $ ld_reg_reg c e
    0x5B -> Op "LD E,E"         $ ld_reg_reg e e
    0x6B -> Op "LD L,E"         $ ld_reg_reg l e
    0x7B -> Op "LD A,E"         $ ld_reg_reg a e
    0x8B -> Op "ADC A,E"        $ Unimplemented
    0x9B -> Op "SBC A,E"        $ Unimplemented
    0xAB -> Op "XOR E"          $ xor_reg e
    0xBB -> Op "CP E"           $ cp_reg cp
    0xCB -> Op "PREFIX CB"      $ Unimplemented
    0xDB -> Op "NOT USED"       $ Unimplemented
    0xEB -> Op "NOT USED"       $ Unimplemented
    0xFB -> Op "EI"             $ ei
    0x0C -> Op "INC C"          $ inc c
    0x1C -> Op "INC E"          $ inc e
    0x2C -> Op "INC L"          $ inc l
    0x3C -> Op "INC A"          $ inc a
    0x4C -> Op "LD C,H"         $ ld_reg_reg c h
    0x5C -> Op "LD E,H"         $ ld_reg_reg e h
    0x6C -> Op "LD L,H"         $ ld_reg_reg l h
    0x7C -> Op "LD A,H"         $ ld_reg_reg a h
    0x8C -> Op "ADC A,H"        $ Unimplemented
    0x9C -> Op "SBC A,H"        $ Unimplemented
    0xAC -> Op "XOR H"          $ xor_reg h
    0xBC -> Op "CP H"           $ cp_reg h
    0xCC -> Op "CALL Z,0x%04X"  $ callIf Zc
    0xDC -> Op "CALL C,0x%04X"  $ callIf Cc
    0xEC -> Op "NOT USED"       $ Unimplemented
    0xFC -> Op "NOT USED"       $ Unimplemented
    0x0D -> Op "DEC C"          $ dec c
    0x1D -> Op "DEC E"          $ dec e
    0x2D -> Op "DEC L"          $ dec l
    0x3D -> Op "DEC A"          $ dec a
    0x4D -> Op "LD C,L"         $ ld_reg_reg c l
    0x5D -> Op "LD E,L"         $ ld_reg_reg e l
    0x6D -> Op "LD L,L"         $ ld_reg_reg l l
    0x7D -> Op "LD A,L"         $ ld_reg_reg a l
    0x8D -> Op "ADC A,L"        $ Unimplemented
    0x9D -> Op "SBC A,L"        $ Unimplemented
    0xAD -> Op "XOR L"          $ xor_reg l
    0xBD -> Op "CP L"           $ cp_reg l
    0xCD -> Op "CALL 0x%04X"    $ call
    0xDD -> Op "NOT USED"       $ Unimplemented
    0xED -> Op "NOT USED"       $ Unimplemented
    0xFD -> Op "NOT USED"       $ Unimplemented
    0x0E -> Op "LD C,0x%02X"    $ ld_reg_d8 c
    0x1E -> Op "LD E,0x%02X"    $ ld_reg_d8 e
    0x2E -> Op "LD L,0x%02X"    $ ld_reg_d8 l
    0x3E -> Op "LD A,0x%02X"    $ ld_reg_d8 a
    0x4E -> Op "LD C,(HL)"      $ ld_reg_deref c HL
    0x5E -> Op "LD E,(HL)"      $ ld_reg_deref e HL
    0x6E -> Op "LD L,(HL)"      $ ld_reg_deref l HL
    0x7E -> Op "LD A,(HL)"      $ ld_reg_deref a HL
    0x8E -> Op "ADC A,(HL)"     $ Unimplemented
    0x9E -> Op "SBC A,(HL)"     $ Unimplemented
    0xAE -> Op "XOR (HL)"       $ Unimplemented
    0xBE -> Op "CP (HL)"        $ Unimplemented
    0xCE -> Op "ADC A,0x%02X"   $ Unimplemented
    0xDE -> Op "SBC A,0x%02X"   $ Unimplemented
    0xEE -> Op "XOR 0x%02X"     $ xor_d8
    0xFE -> Op "CP 0x%02X"      $ cp
    0x0F -> Op "RRCA"           $ Unimplemented
    0x1F -> Op "RRA"            $ Unimplemented
    0x2F -> Op "CPL"            $ cpl
    0x3F -> Op "CCF"            $ Unimplemented
    0x4F -> Op "LD C,A"         $ ld_reg_reg c a
    0x5F -> Op "LD E,A"         $ ld_reg_reg e a
    0x6F -> Op "LD L,A"         $ ld_reg_reg l a 
    0x7F -> Op "LD A,A"         $ ld_reg_reg a a
    0x8F -> Op "ADC A,A"        $ Unimplemented
    0x9F -> Op "SBC A,A"        $ Unimplemented
    0xAF -> Op "XOR A"          $ xor_reg a
    0xBF -> Op "CP A"           $ cp_reg a
    0xCF -> Op "RST 08H"        $ rst 0x08
    0xDF -> Op "RST 18H"        $ rst 0x18
    0xEF -> Op "RST 28H"        $ rst 0x28
    0xFF -> Op "RST 38H"        $ rst 0x38
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
nop :: Instruction s
nop = Ary0 $ return 4

-- JP: Unconditional jump to an address.
jp :: Instruction s
jp = Ary2 $ \addr -> 
    jumpTo addr >> 
    return 16

jpIf :: FlagCondition -> Instruction s
jpIf cc = Ary2 $ \addr ->
    ifM (condition cc)
        (jumpTo addr >> return 16)
        (return 12)

-- OR: Or the contents of A with the argument
-- Flags: Z 0 0 0

orWithA :: Word8 -> CPU s ()
orWithA d8 = do
    modifyReg a (Bit..|. d8)
    va <- readReg a
    setFlags (As (va == 0), Off, Off, Off)

or_d8 :: Instruction s
or_d8 = Ary1 $ \d8 -> do
    orWithA d8
    return 8

or_reg :: CPURegister s Word8 -> Instruction s
or_reg reg = Ary0 $ 
    readReg reg >>= orWithA >> 
    return 4

-- XOR: Exclusive-or the contents of A with the argument.
-- Sets flags: Z 0 0 0
xorWithA :: Word8 -> CPU s ())
xorWithA byte = do
    modifyReg a (Bit.xor byte) 
    -- Set the flags.
    va <- readReg a
    setFlags (As (va == 0), Off, Off, Off)

xor_d8 :: Instruction s
xor_d8 = Ary1 $ \d8 -> do 
    xorWithA d8 
    return 8

xor_reg :: Instruction s
xor_reg reg = Ary0 $ do
    xor =<< readReg reg
    return 4

-- CPL: Complement the bits of A
-- Sets flags: - 1 1 -
cpl :: Instruction s
cpl = Ary0 $
    modifyReg a (Bit.complement) >>
    return 4

-- LD: Load bytes into a destination.
--
-- Lots of variations of this.
-- Some are genuine variations, some are because of my data structures. 
-- (particularly the fact that a ComboRegister is a different type 
--  from the actual 16 bit registers)

ld_reg_d8 :: CPURegister s Word8 -> Instruction s
ld_reg_d8 reg = Ary1 $ \d8 -> do
    writeReg reg d8 
    return 8

ld_reg_reg :: CPURegister s Word8 -> CPURegister s Word8 -> Instruction s
ld_reg_reg to from = Ary0 $ do
    readReg from >>= writeReg to
    return 4

ld_deref_d8 :: ComboRegister -> Instruction s
ld_deref_d8 reg = Ary1 $ \w8 -> do
    derefWrite reg w8
    return 12

ld_deref_reg :: ComboRegister -> CPURegister s Word8 -> Instruction s
ld_deref_reg ptr from = Ary0 $ do
    readReg from >>= derefWrite ptr
    return 8

ld_reg_deref :: CPURegister s Word8 -> ComboRegister -> Instruction s
ld_reg_deref dest ptr = Ary0 $ do
    deref ptr >>= writeReg dest
    return 8

-- LDH (a8),reg: 
-- Load the contents of register into address (a8 + 0xFF00)
ldh_a8_reg :: CPURegister s Word8 -> Instruction s
ldh_a8_reg reg = Ary1 $ \a8 -> do
    let addr = highAddress a8
    readReg reg >>= writeMemory addr
    return 12

-- LDH register,(a8): 
-- Load the contents of (a8 + 0xFF00) into register
ldh_reg_a8 :: CPURegister s Word8 -> Instruction s
ldh_reg_a8 reg = Ary1 $ \a8 -> do
    let addr = highAddress a8
    readMemory addr >>= writeReg reg
    return 12

-- LD (a16),reg:
-- Load the contents of reg into the address (a16)
ld_a16_reg :: CPURegister s Word8 -> Instruction s
ld_a16_reg reg = Ary2 $ \a16 -> do
    readReg reg >>= writeMemory a16
    return 16

ld_reg_a16 :: CPURegister s Word8 -> Instruction s
ld_reg_a16 reg = Ary2 $ \a16 -> do
    readMemory a16 >>= writeReg reg
    return 16

-- LD A,(HL+)
-- Dereference HL, write the value to A, and then increment HL
ld_A_HLPlus :: Instruction s
ld_A_HLPlus = Ary0 $
    writePointerToReg a (pointerPlus HL)

-- LD (C),A
ld_highC_A :: Instruction s
ld_highC_A = Ary0 $ do
    addr <- highAddress <$> (readReg c)
    readReg a >>= writeMemory addr 
    return 8

-- LD A,(C)
ld_A_highC :: Instruction s
ld_A_highC = Ary0 $ do
    addr <- highAddress <$> (readReg c)
    readMemory addr >>= writeReg a
    return 8

ld_reg_d16 :: ComboRegister -> Instruction s
ld_reg_d16 reg = Ary2 $ \d16 ->
    writeComboReg reg d16 >> 
    return 12 

ld_SP_d16 :: Instruction s
ld_SP_d16 = Ary2 $ \d16 ->
    writeReg sp d16 >> 
    return 12

ld_HLMinus_reg :: CPURegister s Word8 -> Instruction s
ld_HLMinus_reg reg = Ary0 $
    writeRegToPointer reg (pointerMinus HL)

ld_HLPlus_reg :: CPURegister s Word8 -> Instruction s
ld_HLPlus_reg reg = Ary0 $
    writeRegToPointer reg (pointerPlus HL)

-- DEC: Decrease a register or memory location by 1
-- Currently just the 8-bit registers.
-- Need to rethink this when I get to the others.
-- Flags: Z 1 H -
dec :: CPURegister s Word8 -> Instruction s
dec reg = Ary0 $ do
    modifyReg reg (subtract 1)
    v <- readReg reg
    setFlags (As (v == 0), On, Off, NA) -- half-carry is complicated, gonna ignore it right now
    return 4

dec16 :: ComboRegister -> Instruction s
dec16 reg = Ary0 $ do
    modifyComboReg reg (subtract 1) 
    return 8

decSP :: Instruction s
decSP = Ary0 $ do
    modifyReg sp (subtract 1)
    return 8

-- Flags Z 1 H -
decDeref :: ComboRegister -> Instruction s
decDeref reg = Ary0 $ do
    derefModify reg (subtract 1)
    v <- readComboReg reg
    setFlags (As (v == 0), On, Off, NA) -- half-carry is complicated, gonna ignore it right now
    return 12

-- Flags: Z 0 H -
inc :: CPURegister s Word8 -> Instruction s
inc reg = Ary0 $ do
    modifyReg reg (+ 1)
    v <- readReg reg
    setFlags (As (v == 0), Off, Off, NA) -- half-carry is complicated, gonna ignore it right now
    return 4

inc16 :: ComboRegister -> Instruction s
inc16 reg = Ary0 $ do
    modifyComboReg reg (+1)
    return 8

incSP :: Instruction s
incSP = Ary0 $ do
    modifyReg sp (+1)
    return 8

-- Flags Z 0 H -
incDeref :: ComboRegister -> Instruction s
incDeref reg = Ary0 $ do
    derefModify reg (+ 1)
    v <- readComboReg reg
    setFlags (As (v == 0), Off, Off, NA) -- half-carry is complicated, gonna ignore it right now
    return 12

-- CP: Compare with A to set flags.
-- Flags: Z 1 H C
compare :: Word8 -> CPU s Cycles
compare byte = do
    av <- readReg a
    let (result, cv) = carriedSubtract av byte 
    setFlags (As (result == 0), On, Off, As cv)
    return 8

cp :: Instruction s
cp = Ary1 $ compare

cp_reg :: CPURegister s Word8 -> Instruction s
cp_reg reg = Ary0 $ compare =<< readReg reg 

-- JR: Relative jump
-- cc is a flag condition. 
-- e is a *signed* 8-bit number.

relativeJump :: Word8 -> CPU s Cycles
relativeJump d8 = 
    modifyReg pc (+ signed) >> return 12
    where signed = fromIntegral $ toSigned d8

jr :: Instruction s
jr = Ary1 $ relativeJump
--
-- If condition is met, PC := PC + e (12 cycles)
-- else                 continue     (8 cycles)
jrIf :: FlagCondition -> Instruction s
jrIf cc = Ary1 $ \d8 -> 
    ifM (condition cc)
        (relativeJump d8)
        (return 8)

push :: ComboRegister -> Instruction s
push reg = Ary0 $ do
    readComboReg reg >>= pushOntoStack
    return 16

pop :: ComboRegister -> Instruction s
pop reg = Ary0 $ do
    popFromStack >>= writeComboReg reg
    return 12

-- CALL
-- Push the current address onto the stack and then jump.
-- This is used to call functions. You can return later
-- by using RET, as long as you haven't done anything else to the stack.

doCall :: Word16 -> CPU s Cycles
doCall a16 = do
    readReg pc >>= pushOntoStack
    jumpTo a16
    return 24

call :: Instruction s
call :: Ary2 $ doCall

callIf :: FlagCondition -> Instruction s
callIf cc = Ary2 $ \addr -> 
    ifM (condition cc)
        (doCall addr)
        (return 12)

-- ReSeT
--
-- RST 0xnn = CALL 0x00nn
--
-- Used to call various hardcoded locations near the start of the ROM,
-- where you can put your useful subroutines.
rst :: Word8 -> Instruction s
rst lowAddr = Ary0 $ do
    doCall (joinBytes lowAddr 0x00)
    return 16

-- RETurn from a function call.
-- The opposite of CALL.
-- Pop the stack and jump to that address.
doRet :: CPU s () 
doRet = do
    addr <- popFromStack
    jumpTo addr

ret :: Instruction s
ret = Ary0 $ 
    doRet >> return 16 -- Confusing statement!

retIf :: FlagCondition -> Instruction s
retIf cc = Ary0 $
    ifM (condition cc)
        (doRet >> return 20)
        (return 8)

-- Return and enable interrupts.
reti :: Instruction s
reti = Ary0 $
    enableMasterInterrupt >> ret
    

-- DI: Disable interrupts
di :: Instruction s
di = Ary0 $ 
    disableMasterInterrupt >> 
    return 4
    
-- EI: Enable interrupts!
ei :: Instruction s
ei = Ary0 $
    enableMasterInterrupt >>
    return 4
