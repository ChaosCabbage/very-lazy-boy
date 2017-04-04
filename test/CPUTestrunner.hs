{-# LANGUAGE Rank2Types #-}

module CPUTestrunner (
    runCPUTest
    ) where

import CPU
import Rom
import Control.Monad.ST

runCPUTest :: (forall s. CPU s a) -> Rom -> a
runCPUTest f rom = runST $ initCPU rom >>= runCPU f
