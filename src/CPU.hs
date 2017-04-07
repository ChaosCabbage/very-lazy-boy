module CPU
    ( 
        CPU
      , runCPU
      , initCPU
      , fetch
      , fetch16

      , readReg
      , writeReg
      , modifyReg
      , readComboReg
      , writeComboReg
      , modifyComboReg
      , readMemory
      , writeMemory
      , modifyMemory

      , Flag(..)
      , readFlag
      , setFlag

    ) where

import Rom
import CPU.Types
import CPU.Environment
import BitTwiddling
import ShowHex

import Control.Monad.ST as ST
import Data.STRef
import Data.Array.ST
import Control.Monad.Reader
import Data.Word
import qualified Data.Bits as Bit

-- CPU computations are 
-- functions from a shared stateful environment into state transformers.
newtype CPU s a = CPU { runCPU :: (CPUEnvironment s) -> ST s a }

instance Monad (CPU s) where
    return x = 
        CPU $ \_ -> return x

    m >>= f = 
        CPU $ \cpu -> do            -- This is now inside the (ST s a) monad.
            current <- runCPU m cpu -- Get the current answer of the ST.
            runCPU (f current) cpu  -- Apply the answer to f. Compose the results.        

instance Applicative (CPU s) where
    pure = return
    (<*>) = ap

instance Functor (CPU s) where
    fmap f m = 
        m >>= return . f

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
    let (low, high) = registerPair reg
    (readReg low) `joinBytesM` (readReg high)

writeComboReg :: ComboRegister -> Word16 -> CPU s ()
writeComboReg reg w = do
    let (lowReg, highReg) = registerPair reg
    let (lowByte, highByte) = toBytes w
    writeReg lowReg lowByte
    writeReg highReg highByte

modifyComboReg :: ComboRegister -> (Word16 -> Word16) -> CPU s ()
modifyComboReg reg f = 
    (readComboReg reg) >>= (writeComboReg reg) . f

-- The memory map is divided into different banks. 
-- Get the bank that this address points to.
-- Some banks are switchable - these are not yet dealt with.
memoryBank :: Address -> MemoryBank s
memoryBank addr
    | addr < 0x0000 = error $ "Memory access out of bounds: " ++ (showHex addr)
    | addr < 0x4000 = rom00   -- 16KB Fixed cartridge rom bank.
    | addr < 0x8000 = rom01   -- 16KB Switchable cartridge rom bank.
    | addr < 0xA000 = vram    -- 8KB Video RAM.
    | addr < 0xC000 = extram  -- 8KB Switchable RAM in cartridge.
    | addr < 0xD000 = wram0   -- 4KB Work RAM.
    | addr < 0xE000 = wram1   -- 4KB Work RAM.
    | addr < 0xFE00 = error $ "Memory access at " ++ (showHex addr) ++ ". " ++
                              "This is an 'echo' address. Not implemented yet :("
    | addr < 0xFEA0 = oam     -- Sprite Attribute Table
    | addr < 0xFF00 = error $ "Memory access at unusable address: " ++ (showHex addr)
    | addr < 0xFF80 = ioports -- IO ports
    | addr < 0xFFFF = hram    -- 127 byte High RAM
    | addr == 0xFFFF = iereg  -- Interrupt Enable register.


readMemory :: Address -> CPU s Word8
readMemory addr = CPU $ \cpu -> 
    let array = memoryBank addr cpu
    in readArray array addr

writeMemory :: Address -> Word8 -> CPU s ()
writeMemory addr byte = CPU $ \cpu -> 
    let array = memoryBank addr cpu
    in writeArray array addr byte

modifyMemory :: Address -> (Word8 -> Word8) -> CPU s ()
modifyMemory addr f = 
    (readMemory addr) >>= (writeMemory addr) . f

incrementPC :: CPU s ()
incrementPC = modifyReg pc (+1)

fetch :: CPU s Opcode
fetch = do
    addr <- readReg pc
    incrementPC 
    readMemory addr

fetch16 :: CPU s Word16
fetch16 = fetch `joinBytesM` fetch


-- The f register also acts as the flags.

data Flag = Z | N | H | C

flagBit :: Flag -> Int
flagBit f = case f of
    Z -> 7
    N -> 6
    H -> 5
    C -> 4

readFlag :: Flag -> CPU s Bool
readFlag flag = do
    flags <- readReg f
    return $ Bit.testBit flags $ flagBit flag

setFlag :: Flag -> Bool -> CPU s ()
setFlag flag b = do
    flags <- readReg f
    let newFlags = Bit.setBit flags $ flagBit flag
    writeReg f newFlags
