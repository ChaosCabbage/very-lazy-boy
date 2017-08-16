module CPU.Interrupts (
    runInterrupts
    ) where

----- Interrupts -----
-- An interrupt is when the program is suddenly sent to a different place.
-- They are triggered by the hardware when something significant happens.

import CPU (
    CPU
  , readMemory
  , modifyMemory
  , isMasterInterruptEnabled
  , disableMasterInterrupt 
  )
import CPU.Environment (Register16(PC))
import CPU.Types (Address)
import CPU.Common (pushOntoStack, jumpTo)
import CPU.Reference (CPUReference(..))
import Data.Bits (setBit, clearBit, testBit)
import Control.Conditional (ifM, (<&&>))

data Interrupt = Interrupt Int Address
-----------------------------------------
vBlank  = Interrupt 0 0x40    -- After the screen has finished drawing
lcdStat = Interrupt 1 0x48    -- Various LCD events
timer   = Interrupt 2 0x50    -- At user-defined time intervals
serial  = Interrupt 3 0x58
joypad  = Interrupt 4 0x60

----- Interrupt Master Enable
--
-- Firstly, no interrupts will ever happen unless the IME is on.

----- Interrupt Flags
--
interruptEnable = 0xFFFF
-- The byte at 0xFFFF represents the _interrupt enable_ flags.
-- If an interrupt's bit is set to 0, the interrupt does not happen.
--
interruptRequest = 0xFF0F
-- The byte at 0xFF0F represents _interrupt requests_.
-- If an interrupt's byte is set, the interrupt should happen 
-- after the next opcode is executed.
--
shouldRun :: Interrupt -> CPU s Bool
shouldRun (Interrupt bit _) =
    isMasterInterruptEnabled <&&>
    (testInterruptEnabled bit) <&&> 
    (testInterruptRequested bit)
--
--
-- As far as I know, if multiple interrupts are waiting, 
-- only the first one is handled, with this priority:
interruptsByPriority = [vBlank, lcdStat, timer, serial, joypad]
--
runInterrupts :: CPU s ()
runInterrupts = tryRemainingInterrupts interruptsByPriority
    where
        tryRemainingInterrupts [] = return ()
        tryRemainingInterrupts (interrupt : remainder) =
            ifM (shouldRun interrupt)
                (interruptRoutine interrupt)
                (tryRemainingInterrupts remainder)
--
-- The interrupt routine takes 12 cycles.
-- It is possible for an interrupt to happen during another interrupt.
interruptRoutine :: Interrupt -> CPU s ()
interruptRoutine (Interrupt bit handler) = do
    disableMasterInterrupt 
    resetInterruptRequest bit
    readWord PC >>= pushOntoStack
    jumpTo handler

testMemoryBit address bit  = fmap (`testBit` bit) (readMemory address) 
setMemoryBit address bit   = modifyMemory address (`setBit` bit)
clearMemoryBit address bit = modifyMemory address (`clearBit` bit)

setInterruptEnable   :: Int -> CPU s ()
resetInterruptEnable :: Int -> CPU s ()
testInterruptEnabled :: Int -> CPU s Bool
setInterruptEnable   = setMemoryBit interruptEnable
resetInterruptEnable = clearMemoryBit interruptEnable
testInterruptEnabled = testMemoryBit interruptEnable

setInterruptRequest   :: Int -> CPU s ()
resetInterruptRequest :: Int -> CPU s ()
testInterruptRequested :: Int -> CPU s Bool
setInterruptRequest   = setMemoryBit interruptRequest
resetInterruptRequest = clearMemoryBit interruptRequest
testInterruptRequested = testMemoryBit interruptRequest

