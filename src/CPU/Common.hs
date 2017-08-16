module CPU.Common (
      fetch
    , fetch16
    , jumpTo
    , pushOntoStack
    , popFromStack
) where

import CPU
import CPU.Environment (Register16(..))
import CPU.Types (Address, Opcode)
import CPU.Reference (CPUReference(..)) 
import BitTwiddling (joinBytes, joinBytesM, toBytes)
import Data.Word (Word16)

----- Other common routines

jumpTo :: Address -> CPU s ()
jumpTo addr = writeWord PC addr

incrementPC :: CPU s ()
incrementPC = modifyWord PC (+1)

-- The Gameboy stack is upside down.
-- It starts at the top and moves down the addresses.
pushOntoStack :: Word16 -> CPU s ()
pushOntoStack word = do
    modifyWord SP (subtract 2 :: Word16 -> Word16)
    let (lowByte, highByte) = toBytes word
    addr <- readWord SP
    writeMemory (addr + 0) lowByte
    writeMemory (addr + 1) highByte

popFromStack :: CPU s Word16
popFromStack = do
    addr <- readWord SP
    lowByte <- readMemory (addr + 0)
    highByte <- readMemory (addr + 1)
    modifyWord SP (+2)
    return (lowByte `joinBytes` highByte)

fetch :: CPU s Opcode
fetch = do
    addr <- readWord PC
    incrementPC 
    readMemory addr

fetch16 :: CPU s Word16
fetch16 = fetch `joinBytesM` fetch

