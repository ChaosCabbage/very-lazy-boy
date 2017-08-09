{-# LANGUAGE MultiParamTypeClasses #-}

module CPU
    ( 
        CPU
      , runCPU
      , initCPU

      , updateMachineTicks

      , readReg
      , writeReg
      , modifyReg
      , readComboReg
      , writeComboReg
      , modifyComboReg
      , readMemory
      , writeMemory
      , modifyMemory
      
      , fetch
      , fetch16
      , jumpTo
      , pushOntoStack

      , enableMasterInterrupt
      , disableMasterInterrupt
      , isMasterInterruptEnabled

      , extractEnvironment

    ) where

import CPU.Types
import CPU.Environment
import qualified CPU.IORegisters as GBIO
import BitTwiddling
import ShowHex

import Control.Monad.ST as ST
import Data.STRef
import Data.Array.ST
import Control.Monad.Reader
import Data.Word

----- The CPU Monad
--
-- I won't explain monads here, because we all know explaining monads is impossible.
-- Anyway,
-- the idea is to represent the internal state of a Gameboy,
-- and build up individual computations on it
-- until you get one big computation.
--
-- Example: Write the contents of register A to memory address 0xD123
-- 
--   readReg a >>= writeMemory 0xD123
--   
-- Super easy. Note that this expression gives you a _computation_.
-- It doesn't do anything by itself.
-- 
-- The CPU monad is implemented on top of the ST monad.
-- The ST monad allows you to have mutable variables and arrays,
-- as long as everything is tidied up at the end and no state leaks out.
-- That's how we achieve suitable speed for gameboy emulation.
-- 
-- A CPU computation is actually a function of the gameboy environment
-- inside the ST monad.
-- See the implementation of `readReg` for a clearer idea.
--
newtype CPU s a = CPU { 
    runCPU :: (CPUEnvironment s) -> ST s a 
}

-- The monad instance is simple since we can lean heavily on the ST monad.
instance Monad (CPU s) where
    return x = 
        CPU $ \_ -> return x

    m >>= f = 
        CPU $ \cpu -> do            -- This is now inside the (ST s a) monad.
            current <- runCPU m cpu -- Get the current answer of the ST.
            runCPU (f current) cpu  -- Apply the answer to f. Compose the results.        

-- Standard Applicative and Functor instances.
instance Applicative (CPU s) where
    pure = return
    (<*>) = ap

instance Functor (CPU s) where
    fmap f m = 
        m >>= return . f

extractEnvironment :: CPU s (CPUEnvironment s)
extractEnvironment = CPU $ \cpu -> return cpu

----- Registers

readReg :: CPURegister s r -> CPU s r
readReg reg = CPU $ \cpu -> readSTRef (reg cpu)

writeReg :: CPURegister s w -> w -> CPU s ()
writeReg reg w = CPU $ \cpu -> writeSTRef (reg cpu) w

modifyReg :: CPURegister s w -> (w -> w) -> CPU s ()
modifyReg reg f = 
    readReg reg >>= (writeReg reg) . f 

-- I feel like the interface for writing to a register,
-- writing to a combo register, and writing to memory
-- could be unified with a typeclass or something, 
-- but I haven't done it yet.

readComboReg :: ComboRegister -> CPU s Word16
readComboReg reg = do
    let (high, low) = registerPair reg
    (readReg low) `joinBytesM` (readReg high)

writeComboReg :: ComboRegister -> Word16 -> CPU s ()
writeComboReg reg w = do
    let (highReg, lowReg) = registerPair reg
    let (lowByte, highByte) = toBytes w
    writeReg lowReg lowByte
    writeReg highReg highByte

modifyComboReg :: ComboRegister -> (Word16 -> Word16) -> CPU s ()
modifyComboReg reg f = 
    (readComboReg reg) >>= (writeComboReg reg) . f


----- Memory
--
-- The memory map is divided into different banks. 
-- Most of them act as simple read/write RAM.
--
-- Some of them don't, such as the IO ports, which
-- actually tell you about things happening in the hardware.
--
-- Some banks are switchable - these are not yet dealt with.
data Addressable s = RAM (MemoryBank s)
                   | IOPorts (IOPorts s)
                   | Sinkhole

readAddressable :: Addressable s -> Address -> CPU s Word8
readAddressable (RAM bank) address = CPU $ \cpu -> 
    readArray (bank cpu) address
readAddressable (IOPorts io) address = CPU $ \cpu -> 
    GBIO.readPort address (io cpu)
readAddressable Sinkhole _ = return 0x00

writeAddressable :: Addressable s -> Address -> Word8 -> CPU s ()
writeAddressable (RAM bank) address byte = CPU $ \cpu -> 
    writeArray (bank cpu) address byte
writeAddressable (IOPorts io) address byte = CPU $ \cpu -> 
    GBIO.writePort address byte (io cpu)
writeAddressable Sinkhole _ _ = return () 

-- Get the bank that this address points to.
memoryBank :: Address -> Addressable s
memoryBank addr 
    | addr < 0x4000 = RAM rom00   -- 16KB Fixed cartridge rom bank.
    | addr < 0x8000 = RAM rom01   -- 16KB Switchable cartridge rom bank.
    | addr < 0xA000 = RAM vram    -- 8KB Video RAM.
    | addr < 0xC000 = RAM extram  -- 8KB Switchable RAM in cartridge.
    | addr < 0xD000 = RAM wram0   -- 4KB Work RAM.
    | addr < 0xE000 = RAM wram1   -- 4KB Work RAM.
    | addr < 0xFE00 = error $ "Memory access at " ++ (showHex addr) ++ ". " ++
                              "This is an 'echo' address. Not implemented yet :("
    | addr < 0xFEA0 = RAM oam     -- Sprite Attribute Table
    | addr < 0xFF00 = Sinkhole
    | addr < 0xFF80 = IOPorts ioports -- IO ports
    | addr < 0xFFFF = RAM hram    -- 127 byte High RAM
    | addr ==0xFFFF = RAM iereg  -- Interrupt Enable register.

-- This is the public function to read a memory address,
-- no matter what it is internally.
readMemory :: Address -> CPU s Word8
readMemory addr = readAddressable (memoryBank addr) addr
--
-- The public function to write to a memory address
writeMemory :: Address -> Word8 -> CPU s ()
writeMemory addr = writeAddressable (memoryBank addr) addr
--
-- A lot of instructions want to modify an memory address in-place,
-- so here's a convenient function
modifyMemory :: Address -> (Word8 -> Word8) -> CPU s ()
modifyMemory addr f = 
    (readMemory addr) >>= (writeMemory addr) . f

----- Interrupt Master Enable
--
-- No interrupts will ever happen unless the IME is on.
-- This is not a memory address. It's either on or off.
enableMasterInterrupt  :: CPU s ()
disableMasterInterrupt :: CPU s ()
enableMasterInterrupt  = CPU $ \cpu -> writeSTRef (ime cpu) True
disableMasterInterrupt = CPU $ \cpu -> writeSTRef (ime cpu) False

isMasterInterruptEnabled :: CPU s Bool
isMasterInterruptEnabled = CPU $ \cpu -> readSTRef (ime cpu)

----- Other common routines

jumpTo :: Address -> CPU s ()
jumpTo addr = writeReg pc addr

incrementPC :: CPU s ()
incrementPC = modifyReg pc (+1)

-- The Gameboy stack is upside down.
-- It starts at the top and moves down the addresses.
pushOntoStack :: Word16 -> CPU s ()
pushOntoStack word = do
    let (lowByte, highByte) = toBytes word
    addr <- readReg sp
    writeMemory (addr + 0) lowByte
    writeMemory (addr + 1) highByte
    modifyReg sp (subtract 2)

fetch :: CPU s Opcode
fetch = do
    addr <- readReg pc
    incrementPC 
    readMemory addr

fetch16 :: CPU s Word16
fetch16 = fetch `joinBytesM` fetch

----- Machine ticks
--
-- Keep the machine ticking along in between instructions.
updateMachineTicks :: Cycles -> CPU s ()
updateMachineTicks extraCycles = CPU $ \cpu ->
    GBIO.addCycles (ioports cpu) extraCycles
