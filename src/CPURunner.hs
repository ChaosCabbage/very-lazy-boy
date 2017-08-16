module CPURunner (
    step
    ) where

import CPU
import CPU.Instructions
import CPU.Interrupts
import CPU.Types 
import CPU.Common (fetch)

step :: CPU s Cycles
step = do
    op <- instruction.opTable <$> fetch
    cycles <- execute op
    updateMachineTicks cycles
    runInterrupts
    return cycles