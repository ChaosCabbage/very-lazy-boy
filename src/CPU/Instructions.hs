{-# LANGUAGE FlexibleContexts #-}
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
import CPU.Reference
import CPU.Pointer
import CPU.Common
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
    0x40 -> Op "LD B,B"         $ ld_reg_reg B B
    0x50 -> Op "LD D,B"         $ ld_reg_reg D B
    0x60 -> Op "LD H,B"         $ ld_reg_reg H B
    0x70 -> Op "LD (HL),B"      $ ld_pointer_reg hlPointer B
    0x80 -> Op "ADD A,B"        $ add_reg B
    0x90 -> Op "SUB B"          $ Unimplemented
    0xA0 -> Op "AND B"          $ and_reg B
    0xB0 -> Op "OR B"           $ or_reg B
    0xC0 -> Op "RET NZ"         $ retIf NZc
    0xD0 -> Op "RET NC"         $ retIf NCc
    0xE0 -> Op "LDH (0x%02X),A" $ ldh_a8_reg A
    0xF0 -> Op "LDH A,(0x%02X)" $ ldh_reg_a8 A    
    0x01 -> Op "LD BC,0x%04X"   $ ld_reg16_d16 BC
    0x11 -> Op "LD DE,0x%04X"   $ ld_reg16_d16 DE
    0x21 -> Op "LD HL,0x%04X"   $ ld_reg16_d16 HL
    0x31 -> Op "LD SP,0x%04X"   $ ld_reg16_d16 SP
    0x41 -> Op "LD B,C"         $ ld_reg_reg B C
    0x51 -> Op "LD D,C"         $ ld_reg_reg D C
    0x61 -> Op "LD H,C"         $ ld_reg_reg H C
    0x71 -> Op "LD (HL),C"      $ ld_pointer_reg hlPointer C
    0x81 -> Op "ADD A,C"        $ Unimplemented
    0x91 -> Op "SUB C"          $ Unimplemented
    0xA1 -> Op "AND C"          $ and_reg C
    0xB1 -> Op "OR C"           $ or_reg C    
    0xC1 -> Op "POP BC"         $ pop BC
    0xD1 -> Op "POP DE"         $ pop DE
    0xE1 -> Op "POP HL"         $ pop HL
    0xF1 -> Op "POP AF"         $ pop AF
    0x02 -> Op "LD (BC),A"      $ ld_pointer_reg (pointerReg BC) A
    0x12 -> Op "LD (DE),A"      $ ld_pointer_reg (pointerReg DE) A
    0x22 -> Op "LD (HL+),A"     $ ld_pointer_reg hlPlus A
    0x32 -> Op "LD (HL-),A"     $ ld_pointer_reg hlMinus A
    0x42 -> Op "LD B,D"         $ ld_reg_reg B D
    0x52 -> Op "LD D,D"         $ ld_reg_reg D D
    0x62 -> Op "LD H,D"         $ ld_reg_reg H D
    0x72 -> Op "LD (HL),D"      $ ld_pointer_reg hlPointer D
    0x82 -> Op "ADD A,D"        $ Unimplemented
    0x92 -> Op "SUB D"          $ Unimplemented
    0xA2 -> Op "AND D"          $ and_reg D
    0xB2 -> Op "OR D"           $ or_reg D
    0xC2 -> Op "JP NZ,0x%04X"   $ jpIf NZc
    0xD2 -> Op "JP NC,0x%04X"   $ jpIf NCc
    0xE2 -> Op "LD (C),A"       $ ld_highC_A
    0xF2 -> Op "LD A,(C)"       $ ld_A_highC     
    0x03 -> Op "INC BC"         $ inc16 BC
    0x13 -> Op "INC DE"         $ inc16 DE
    0x23 -> Op "INC HL"         $ inc16 HL
    0x33 -> Op "INC SP"         $ inc16 SP
    0x43 -> Op "LD B,E"         $ ld_reg_reg B E
    0x53 -> Op "LD D,E"         $ ld_reg_reg D E
    0x63 -> Op "LD H,E"         $ ld_reg_reg H E
    0x73 -> Op "LD (HL),E"      $ ld_pointer_reg hlPointer E
    0x83 -> Op "ADD A,E"        $ Unimplemented
    0x93 -> Op "SUB E"          $ Unimplemented
    0xA3 -> Op "AND E"          $ and_reg E
    0xB3 -> Op "OR E"           $ or_reg E
    0xC3 -> Op "JP 0x%04X"      $ jp
    0xD3 -> Op "NOT USED"       $ Unimplemented
    0xE3 -> Op "NOT USED"       $ Unimplemented
    0xF3 -> Op "DI"             $ di
    0x04 -> Op "INC B"          $ inc B
    0x14 -> Op "INC D"          $ inc D
    0x24 -> Op "INC H"          $ inc H
    0x34 -> Op "INC (HL)"       $ incDeref hlPointer
    0x44 -> Op "LD B,H"         $ ld_reg_reg B H
    0x54 -> Op "LD D,H"         $ ld_reg_reg D H
    0x64 -> Op "LD H,H"         $ ld_reg_reg H H
    0x74 -> Op "LD (HL),H"      $ Unimplemented
    0x84 -> Op "ADD A,H"        $ Unimplemented
    0x94 -> Op "SUB H"          $ Unimplemented
    0xA4 -> Op "AND H"          $ Unimplemented
    0xB4 -> Op "OR H"           $ or_reg H
    0xC4 -> Op "CALL NZ,0x%04X" $ callIf NZc
    0xD4 -> Op "CALL NC,0x%04X" $ callIf NCc
    0xE4 -> Op "NOT USED"       $ Unimplemented
    0xF4 -> Op "NOT USED"       $ Unimplemented
    0x05 -> Op "DEC B"          $ dec B
    0x15 -> Op "DEC D"          $ dec D
    0x25 -> Op "DEC H"          $ dec H
    0x35 -> Op "DEC (HL)"       $ decDeref hlPointer
    0x45 -> Op "LD B,L"         $ ld_reg_reg B L
    0x55 -> Op "LD D,L"         $ ld_reg_reg D L
    0x65 -> Op "LD H,L"         $ ld_reg_reg H L
    0x75 -> Op "LD (HL),L"      $ ld_pointer_reg hlPointer L
    0x85 -> Op "ADD A,L"        $ Unimplemented
    0x95 -> Op "SUB L"          $ Unimplemented
    0xA5 -> Op "AND L"          $ and_reg L
    0xB5 -> Op "OR L"           $ or_reg L
    0xC5 -> Op "PUSH BC"        $ push BC
    0xD5 -> Op "PUSH DE"        $ push DE
    0xE5 -> Op "PUSH HL"        $ push HL
    0xF5 -> Op "PUSH AF"        $ push AF
    0x06 -> Op "LD B,0x%02X"    $ ld_reg_d8 B
    0x16 -> Op "LD D,0x%02X"    $ ld_reg_d8 D
    0x26 -> Op "LD H,0x%02X"    $ ld_reg_d8 H
    0x36 -> Op "LD (HL),0x%02X" $ ld_pointer_d8 hlPointer
    0x46 -> Op "LD B,(HL)"      $ ld_reg_pointer B hlPointer
    0x56 -> Op "LD D,(HL)"      $ ld_reg_pointer D hlPointer
    0x66 -> Op "LD H,(HL)"      $ ld_reg_pointer H hlPointer
    0x76 -> Op "HALT"           $ Unimplemented
    0x86 -> Op "ADD A,(HL)"     $ Unimplemented
    0x96 -> Op "SUB (HL)"       $ Unimplemented
    0xA6 -> Op "AND (HL)"       $ and_deref hlPointer
    0xB6 -> Op "OR (HL)"        $ Unimplemented
    0xC6 -> Op "ADD A,0x%02X"   $ Unimplemented
    0xD6 -> Op "SUB 0x%02X"     $ Unimplemented
    0xE6 -> Op "AND 0x%02X"     $ and_d8
    0xF6 -> Op "OR 0x%02X"      $ or_d8
    0x07 -> Op "RLCA"           $ Unimplemented
    0x17 -> Op "RLA"            $ Unimplemented
    0x27 -> Op "DAA"            $ Unimplemented
    0x37 -> Op "SCF"            $ Unimplemented
    0x47 -> Op "LD B,A"         $ ld_reg_reg B A
    0x57 -> Op "LD D,A"         $ ld_reg_reg D A
    0x67 -> Op "LD H,A"         $ ld_reg_reg H A
    0x77 -> Op "LD (HL),A"      $ ld_pointer_reg hlPointer A
    0x87 -> Op "ADD A,A"        $ Unimplemented
    0x97 -> Op "SUB A"          $ Unimplemented
    0xA7 -> Op "AND A"          $ and_reg A
    0xB7 -> Op "OR A"           $ or_reg A
    0xC7 -> Op "RST 00H"        $ rst 0x00
    0xD7 -> Op "RST 10H"        $ rst 0x10
    0xE7 -> Op "RST 20H"        $ rst 0x20
    0xF7 -> Op "RST 30H"        $ rst 0x30
    0x08 -> Op "LD (0x%04X),SP" $ Unimplemented
    0x18 -> Op "JR 0x%02X"      $ jr
    0x28 -> Op "JR C,0x%02X"    $ jrIf Zc
    0x38 -> Op "JR C,0x%02X"    $ jrIf Cc
    0x48 -> Op "LD C,B"         $ ld_reg_reg C B
    0x58 -> Op "LD E,B"         $ ld_reg_reg E B
    0x68 -> Op "LD L,B"         $ ld_reg_reg L B
    0x78 -> Op "LD A,B"         $ ld_reg_reg A B
    0x88 -> Op "ADC A,B"        $ Unimplemented
    0x98 -> Op "SBC A,B"        $ Unimplemented
    0xA8 -> Op "XOR B"          $ xor_reg B
    0xB8 -> Op "CP B"           $ cp_reg B
    0xC8 -> Op "RET Z"          $ retIf Zc
    0xD8 -> Op "RET C"          $ retIf Cc
    0xE8 -> Op "ADD SP,0x%02X"  $ add_SP_r8
    0xF8 -> Op "LD HL,SP+0x%02X"$ Unimplemented
    0x09 -> Op "ADD HL,BC"      $ add_HL_reg16 BC
    0x19 -> Op "ADD HL,DE"      $ add_HL_reg16 DE
    0x29 -> Op "ADD HL,HL"      $ add_HL_reg16 HL
    0x39 -> Op "ADD HL,SP"      $ add_HL_reg16 SP
    0x49 -> Op "LD C,C"         $ ld_reg_reg C C
    0x59 -> Op "LD E,C"         $ ld_reg_reg E C
    0x69 -> Op "LD L,C"         $ ld_reg_reg L C
    0x79 -> Op "LD A,C"         $ ld_reg_reg A C
    0x89 -> Op "ADC A,C"        $ Unimplemented
    0x99 -> Op "SBC A,C"        $ Unimplemented
    0xA9 -> Op "XOR C"          $ xor_reg C
    0xB9 -> Op "CP C"           $ cp_reg C
    0xC9 -> Op "RET"            $ ret
    0xD9 -> Op "RETI"           $ reti
    0xE9 -> Op "JP (HL)"        $ jp_HL
    0xF9 -> Op "LD SP,HL"       $ ld_SP_HL
    0x0A -> Op "LD A,(BC)"      $ ld_reg_pointer A (pointerReg BC)
    0x1A -> Op "LD A,(DE)"      $ ld_reg_pointer A (pointerReg DE)
    0x2A -> Op "LD A,(HL+)"     $ ld_reg_pointer A hlPlus
    0x3A -> Op "LD A,(HL-)"     $ ld_reg_pointer A hlMinus
    0x4A -> Op "LD C,D"         $ ld_reg_reg C D
    0x5A -> Op "LD E,D"         $ ld_reg_reg E D
    0x6A -> Op "LD L,D"         $ ld_reg_reg L D
    0x7A -> Op "LD A,D"         $ ld_reg_reg A D
    0x8A -> Op "ADC A,D"        $ Unimplemented
    0x9A -> Op "SBC A,D"        $ Unimplemented
    0xAA -> Op "XOR D"          $ xor_reg D
    0xBA -> Op "CP D"           $ cp_reg D
    0xCA -> Op "JP Z,0x%04X"    $ jpIf Zc
    0xDA -> Op "JP C,0x%04X"    $ jpIf Cc
    0xEA -> Op "LD (0x%04X),A"  $ ld_a16_reg A
    0xFA -> Op "LD A,(0x%04X)"  $ ld_reg_a16 A
    0x0B -> Op "DEC BC"         $ dec16 BC
    0x1B -> Op "DEC DE"         $ dec16 DE
    0x2B -> Op "DEC HL"         $ dec16 HL
    0x3B -> Op "DEC SP"         $ dec16 SP
    0x4B -> Op "LD C,E"         $ ld_reg_reg C E
    0x5B -> Op "LD E,E"         $ ld_reg_reg E E
    0x6B -> Op "LD L,E"         $ ld_reg_reg L E
    0x7B -> Op "LD A,E"         $ ld_reg_reg A E
    0x8B -> Op "ADC A,E"        $ Unimplemented
    0x9B -> Op "SBC A,E"        $ Unimplemented
    0xAB -> Op "XOR E"          $ xor_reg E
    0xBB -> Op "CP E"           $ cp_reg E
    0xCB -> Op "CB (%02X)"      $ cb
    0xDB -> Op "NOT USED"       $ Unimplemented
    0xEB -> Op "NOT USED"       $ Unimplemented
    0xFB -> Op "EI"             $ ei
    0x0C -> Op "INC C"          $ inc C
    0x1C -> Op "INC E"          $ inc E
    0x2C -> Op "INC L"          $ inc L
    0x3C -> Op "INC A"          $ inc A
    0x4C -> Op "LD C,H"         $ ld_reg_reg C H
    0x5C -> Op "LD E,H"         $ ld_reg_reg E H
    0x6C -> Op "LD L,H"         $ ld_reg_reg L H
    0x7C -> Op "LD A,H"         $ ld_reg_reg A H
    0x8C -> Op "ADC A,H"        $ Unimplemented
    0x9C -> Op "SBC A,H"        $ Unimplemented
    0xAC -> Op "XOR H"          $ xor_reg H
    0xBC -> Op "CP H"           $ cp_reg H
    0xCC -> Op "CALL Z,0x%04X"  $ callIf Zc
    0xDC -> Op "CALL C,0x%04X"  $ callIf Cc
    0xEC -> Op "NOT USED"       $ Unimplemented
    0xFC -> Op "NOT USED"       $ Unimplemented
    0x0D -> Op "DEC C"          $ dec C
    0x1D -> Op "DEC E"          $ dec E
    0x2D -> Op "DEC L"          $ dec L
    0x3D -> Op "DEC A"          $ dec A
    0x4D -> Op "LD C,L"         $ ld_reg_reg C L
    0x5D -> Op "LD E,L"         $ ld_reg_reg E L
    0x6D -> Op "LD L,L"         $ ld_reg_reg L L
    0x7D -> Op "LD A,L"         $ ld_reg_reg A L
    0x8D -> Op "ADC A,L"        $ Unimplemented
    0x9D -> Op "SBC A,L"        $ Unimplemented
    0xAD -> Op "XOR L"          $ xor_reg L
    0xBD -> Op "CP L"           $ cp_reg L
    0xCD -> Op "CALL 0x%04X"    $ call
    0xDD -> Op "NOT USED"       $ Unimplemented
    0xED -> Op "NOT USED"       $ Unimplemented
    0xFD -> Op "NOT USED"       $ Unimplemented
    0x0E -> Op "LD C,0x%02X"    $ ld_reg_d8 C
    0x1E -> Op "LD E,0x%02X"    $ ld_reg_d8 E
    0x2E -> Op "LD L,0x%02X"    $ ld_reg_d8 L
    0x3E -> Op "LD A,0x%02X"    $ ld_reg_d8 A
    0x4E -> Op "LD C,(HL)"      $ ld_reg_pointer C hlPointer
    0x5E -> Op "LD E,(HL)"      $ ld_reg_pointer E hlPointer
    0x6E -> Op "LD L,(HL)"      $ ld_reg_pointer L hlPointer
    0x7E -> Op "LD A,(HL)"      $ ld_reg_pointer A hlPointer
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
    0x4F -> Op "LD C,A"         $ ld_reg_reg C A
    0x5F -> Op "LD E,A"         $ ld_reg_reg E A
    0x6F -> Op "LD L,A"         $ ld_reg_reg L A 
    0x7F -> Op "LD A,A"         $ ld_reg_reg A A
    0x8F -> Op "ADC A,A"        $ Unimplemented
    0x9F -> Op "SBC A,A"        $ Unimplemented
    0xAF -> Op "XOR A"          $ xor_reg A
    0xBF -> Op "CP A"           $ cp_reg A
    0xCF -> Op "RST 08H"        $ rst 0x08
    0xDF -> Op "RST 18H"        $ rst 0x18
    0xEF -> Op "RST 28H"        $ rst 0x28
    0xFF -> Op "RST 38H"        $ rst 0x38
    _    -> Op "???" Unimplemented

-- The extended instruction set.
-- Opcode CB means the next op comes from here.
cbTable :: Opcode -> Operation s
cbTable opcode = case opcode of
    0x37 -> Op "SWAP A" $ swap_reg A
    _    -> Op "???" Unimplemented

-- Not quite sure about how to get the descriptions
-- of the extended instructions
-- back up to the debugger with the current structure.
cb :: Instruction s
cb = Ary1 $ execute . instruction . cbTable 


execute :: Instruction s -> CPU s Cycles
execute (Ary0 op) = op
execute (Ary1 op) = fetch >>= op
execute (Ary2 op) = fetch16 >>= op
execute Unimplemented = error "Trying to execute unimplemented opcode"

-- Flag conditions, for instructions such as "JP NZ,A"

data FlagCondition = Cc | NCc | Zc | NZc | Any

condition :: FlagCondition -> CPU s Bool
condition fc = case fc of
    Cc  -> readFlag Cf
    NCc -> fmap not (readFlag Cf)
    Zc  -> readFlag Zf
    NZc -> fmap not (readFlag Zf)
    Any -> return True


setZFlag :: CPU s Word8 -> CPU s ()
setZFlag =
    (setFlagM Zf) . (fmap (==0)) 

-- Several instructions add 0xFF00 to their argument
-- to get an address.
highAddress :: Word8 -> Address
highAddress a8 = 
    0xFF00 + (fromIntegral a8 :: Word16)

highPointer :: Word8 -> CPUPointer s
highPointer = 
    pointer . highAddress

hlPointer :: CPUPointer s
hlPointer = pointerReg HL

hlPlus :: CPUPointer s
hlPlus = pointerPlus HL

hlMinus :: CPUPointer s
hlMinus = pointerMinus HL

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

-- JP (HL)
jp_HL :: Instruction s 
jp_HL = Ary0 $ do
    jumpTo =<< readWord HL
    return 4

-- AND: Bitwise and the contents of A with the argument.
-- Flags: Z 0 1 0
andWithA :: Word8 -> CPU s ()
andWithA d8 = do
    modifyWord A (Bit..&. d8)
    setFlags (NA, Off, On, Off)
    setZFlag $ readWord A

and_reg :: Register8 -> Instruction s
and_reg reg = Ary0 $ do
    readWord reg >>= andWithA
    return 4

and_d8 :: Instruction s
and_d8 = Ary1 $ \d8 -> do
    andWithA d8
    return 8

and_deref :: CPUPointer s -> Instruction s
and_deref ptr = Ary0 $ do
    readPtr ptr >>= andWithA
    return 8

-- OR: Or the contents of A with the argument
-- Flags: Z 0 0 0

orWithA :: Word8 -> CPU s ()
orWithA d8 = do
    modifyWord A (Bit..|. d8)
    setFlags (NA, Off, Off, Off)
    setZFlag $ readWord A

or_d8 :: Instruction s
or_d8 = Ary1 $ \d8 -> do
    orWithA d8
    return 8

or_reg :: Register8 -> Instruction s
or_reg reg = Ary0 $ 
    readWord reg >>= orWithA >> 
    return 4

-- XOR: Exclusive-or the contents of A with the argument.
-- Sets flags: Z 0 0 0
xorWithA :: Word8 -> CPU s ()
xorWithA byte = do
    modifyWord A (Bit.xor byte) 
    setFlags (Off, Off, Off, Off)
    setZFlag $ readWord A

xor_d8 :: Instruction s
xor_d8 = Ary1 $ \d8 -> do 
    xorWithA d8 
    return 8

xor_reg :: Register8 -> Instruction s
xor_reg reg = Ary0 $ do
    xorWithA =<< readWord reg
    return 4

-- CPL: Complement the bits of A
-- Sets flags: - 1 1 -
cpl :: Instruction s
cpl = Ary0 $
    modifyWord A (Bit.complement) >>
    return 4

-- LD: Load bytes into a destination.
--
-- Lots of variations of this.
-- Some are genuine variations, some are because of my data structures. 
-- (particularly the fact that a ComboRegister is a different type 
--  from the actual 16 bit registers)

ld :: (CPUReference s r1 w, CPUReference s r2 w) => r1 -> r2 -> CPU s ()
ld to from = readWord from >>= writeWord to

ld_pointer_x :: (CPUReference s r Word8) => CPUPointer s -> r -> CPU s ()
ld_pointer_x to from = 
    readWord from >>= writeToPtr to

ld_x_pointer :: (CPUReference s r Word8) => r -> CPUPointer s -> CPU s ()
ld_x_pointer to from = 
    readPtr from >>= writeWord to

ld_reg_d8 :: Register8 -> Instruction s
ld_reg_d8 reg = Ary1 $ \d8 -> do
    writeWord reg d8 
    return 8

ld_reg_reg :: Register8 -> Register8 -> Instruction s
ld_reg_reg to from = Ary0 $ do
    ld to from
    return 4

ld_pointer_d8 :: CPUPointer s -> Instruction s
ld_pointer_d8 p = Ary1 $ \w8 -> do
    writeToPtr p w8
    return 12

ld_reg_pointer :: Register8 -> CPUPointer s -> Instruction s
ld_reg_pointer to from = Ary0 $ do
    ld_x_pointer to from
    return 8

ld_pointer_reg :: CPUPointer s -> Register8 -> Instruction s
ld_pointer_reg to from = Ary0 $ do
    ld_pointer_x to from
    return 8

-- LDH (a8),reg: 
-- Load the contents of register into address (a8 + 0xFF00)
ldh_a8_reg :: Register8 -> Instruction s
ldh_a8_reg reg = Ary1 $ \a8 -> do
    ld_pointer_x (highPointer a8) reg
    return 12

-- LDH register,(a8): 
-- Load the contents of (a8 + 0xFF00) into register
ldh_reg_a8 :: Register8 -> Instruction s
ldh_reg_a8 reg = Ary1 $ \a8 -> do
    ld_x_pointer reg (highPointer a8)
    return 12

-- LD (a16),reg:
-- Load the contents of reg into the address (a16)
ld_a16_reg :: Register8 -> Instruction s
ld_a16_reg reg = Ary2 $ \a16 -> do
    readWord reg >>= writeMemory a16
    return 16

ld_reg_a16 :: Register8 -> Instruction s
ld_reg_a16 reg = Ary2 $ \a16 -> do
    readMemory a16 >>= writeWord reg
    return 16

-- LD (C),A
ld_highC_A :: Instruction s
ld_highC_A = Ary0 $ do
    ptr <- highPointer <$> (readWord C)
    ld_pointer_x ptr A 
    return 8

-- LD A,(C)
ld_A_highC :: Instruction s
ld_A_highC = Ary0 $ do
    addr <- highPointer <$> (readWord C)
    ld_x_pointer A addr
    return 8

ld_reg16_d16 :: (CPUReference s r Word16) => r -> Instruction s
ld_reg16_d16 reg = Ary2 $ \d16 -> do
    writeWord reg d16
    return 12 

ld_SP_HL :: Instruction s
ld_SP_HL = Ary0 $ do
    ld SP HL
    return 8

-- ADD: Add stuff to A. Pretty obvious.
-- Set flags Z 0 H C
addToA :: Word8 -> CPU s ()
addToA d8 = do
    aValue <- readWord A
    let (answer, didCarry, didHalfCarry) = carriedAdd aValue d8
    writeWord A answer  
    setFlags (As (answer == 0), Off, As didHalfCarry, As didCarry)

add_reg :: Register8 -> Instruction s
add_reg reg = Ary0 $ do
    readWord reg >>= addToA
    return 4
    
add_d8 :: Instruction s
add_d8 = Ary1 $ \d8 -> do
    addToA d8
    return 8

add_deref :: CPUPointer s -> Instruction s
add_deref ptr = Ary0 $ do
    readPtr ptr >>= addToA
    return 8

-- ADD 16-bit
-- Set flags: - 0 H C
addToHL :: Word16 -> CPU s ()
addToHL value = do
    hlValue <- readComboReg HL
    let (answer, didCarry, didHalfCarry) = carriedAdd hlValue value
    writeComboReg HL answer
    setFlags (NA, Off, As didHalfCarry, As didCarry)

add_HL_reg16 :: (CPUReference s r Word16) => r -> Instruction s
add_HL_reg16 reg = Ary0 $ do
    readWord reg >>= addToHL
    return 8

-- ADD SP,r8
-- Add a *SIGNED* 8-bit number to the SP register.
-- Flags: 0 0 H C
add_SP_r8 :: Instruction s
add_SP_r8 = Ary1 $ \r8 -> do
    spValue <- readWord SP
    let (answer, didCarry, didHalfCarry) = signedAdd spValue r8
    writeWord SP answer
    setFlags (Off, Off, As didHalfCarry, As didCarry)
    return 16

-- SUB: Subtract from A
-- Set flags: Z 1 H C
subtractFromA :: Word8 -> CPU s ()
subtractFromA d8 = do
    aValue <- readWord A
    let (answer, didCarry, didHalfCarry) = carriedSubtract aValue d8
    writeWord A answer
    setFlags (As (answer == 0), On, As didHalfCarry, As didCarry)

sub_reg :: Register8 -> Instruction s
sub_reg reg = Ary0 $ do
    readWord reg >>= subtractFromA
    return 4

sub_d8 :: Instruction s
sub_d8 = Ary1 $ \d8 -> do
    subtractFromA d8
    return 8

sub_deref :: CPUPointer s -> Instruction s
sub_deref ptr = Ary0 $ do
    readPtr ptr >>= subtractFromA
    return 8

-- DEC: Decrease a register or memory location by 1
-- Currently just the 8-bit registers.
-- Need to rethink this when I get to the others.
-- Flags: Z 1 H -
dec :: Register8 -> Instruction s
dec reg = Ary0 $ do
    modifyWord reg (subtract 1)
    setFlags (NA, On, Off, NA) -- half-carry is complicated, gonna ignore it right now
    setZFlag $ readWord reg
    return 4

dec16 :: (CPUReference s a Word16) => a -> Instruction s
dec16 ref = Ary0 $ do
    modifyWord ref (subtract 1 :: Word16 -> Word16) 
    return 8

-- Flags Z 1 H -
decDeref :: CPUPointer s -> Instruction s
decDeref ptr = Ary0 $ do
    modifyThroughPtr ptr (subtract 1)
    setFlags (NA, On, Off, NA) -- half-carry is complicated, gonna ignore it right now
    setZFlag $ readPtr ptr
    return 12

-- INC 8-bit registers or memory0
-- Flags: Z 0 H -

increaseReference :: (CPUReference s a Word8) => a -> CPU s ()
increaseReference ref = do
    val <- readWord ref
    let (answer, _, didHalfCarry) = carriedAdd val 1
    writeWord ref answer
    setFlags (As (answer == 0), Off, As didHalfCarry, NA)

-- The whole point of the CPUReference type class was to implement
-- these two functions with the same code ^ v
-- Unfortunately, I can't get the CPUPointer instance to work.

increaseDeref :: CPUPointer s -> CPU s ()
increaseDeref ref = do
    val <- readPtr ref
    let (answer, _, didHalfCarry) = carriedAdd val 1
    writeToPtr ref answer
    setFlags (As (answer == 0), Off, As didHalfCarry, NA)

inc :: Register8 -> Instruction s
inc reg = Ary0 $ do
    increaseReference reg
    return 4

-- Flags Z 0 H -
incDeref :: CPUPointer s -> Instruction s
incDeref ptr = Ary0 $ do
    increaseDeref ptr
    return 12

-- No flags
inc16 :: (CPUReference s a Word16) => a -> Instruction s
inc16 reg = Ary0 $ do
    modifyWord reg ((+1) :: Word16 -> Word16)
    return 8

-- CP: Compare with A to set flags.
-- Flags: Z 1 H C
compareWithA :: Word8 -> CPU s Cycles
compareWithA byte = do
    av <- readWord A
    let (result, didCarry, didHalfCarry) = carriedSubtract av byte 
    setFlags (As (result == 0), On, As didHalfCarry, As didCarry)
    return 8

cp :: Instruction s
cp = Ary1 $ compareWithA

cp_reg :: Register8 -> Instruction s
cp_reg reg = Ary0 $ compareWithA =<< readWord reg 

-- JR: Relative jump
-- cc is a flag condition. 
-- e is a *signed* 8-bit number.

relativeJump :: Word8 -> CPU s Cycles
relativeJump d8 = 
    modifyWord PC (+ signed) >> return 12
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
    readWord PC >>= pushOntoStack
    jumpTo a16
    return 24

call :: Instruction s
call = Ary2 $ doCall

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
reti = Ary0 $ do
    enableMasterInterrupt 
    doRet
    return 16
    

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

-- SWAP : Swap the nybbles.
swap_reg :: Register8 -> Instruction s
swap_reg reg = Ary0 $ do
    modifyWord reg swapNybbles
    setFlags (NA, Off, Off, Off)
    setZFlag $ readWord reg
    return 8
