module Main where

import CPU
import CPURunner
import CPU.Environment
import CPU.FrozenEnvironment

import Control.Monad.ST

import Text.Printf

main :: IO ()
main = putStrLn $ "A = " ++ (show $ frz_a $ runThing defaultCPU)

runThing :: FrozenCPUEnvironment -> FrozenCPUEnvironment
runThing initialEnv = 
    runST $ resumeCPU initialEnv >>= runCPU (
        changeA >>
        extractEnvironment
    ) >>=
    pauseCPU

changeA :: CPU s ()
changeA = writeReg a 134

