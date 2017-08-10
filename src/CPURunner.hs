{-# LANGUAGE Rank2Types #-}

module CPURunner (
    runCPUTest,
    step
    ) where

import CPU
import CPU.Instructions
import CPU.Interrupts
import CPU.Types 
import Control.Monad.ST

runCPUTest :: (forall s. CPU s a) -> Memory -> a
runCPUTest f rom = runST $ initCPU rom >>= runCPU f    

step :: CPU s Cycles
step = do
    op <- instruction.opTable <$> fetch
    cycles <- execute op
    updateMachineTicks cycles
    runInterrupts
    return cycles