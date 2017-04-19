{-# LANGUAGE Rank2Types #-}

module CPURunner (
    runCPUTest
    ) where

import CPU
import CPU.Types
import Control.Monad.ST

runCPUTest :: (forall s. CPU s a) -> Memory -> a
runCPUTest f rom = runST $ initCPU rom >>= runCPU f
