module Main where

import CPU
import CPURunner
import CPU.Environment
import CPU.FrozenEnvironment
import CPU.Instructions
import Cartridge

import Control.Monad.ST
import Data.Array
import Text.Printf

main :: IO ()
main = do
    rom <- romFromFile
    let cpuState = defaultCPU { frz_rom00 = rom }
    stepper cpuState

stepper :: FrozenCPUEnvironment -> IO ()
stepper cpuState = do
    let pc = frz_pc cpuState
    let byte = frz_rom00 cpuState ! pc
    putStrLn $ printf "PC = 0x%02X (0x%02X)" pc byte
    putStrLn "Type 'STEP' or 'QUIT' > "
    command <- getLine
    case command of
        "STEP" -> stepper $ runStep cpuState
        "QUIT" -> putStrLn "Byee"
        _ -> putStrLn "Unknown command" >> stepper cpuState

runStep :: FrozenCPUEnvironment -> FrozenCPUEnvironment
runStep initialEnv = 
    runST $ resumeCPU initialEnv >>= runCPU (
        step >>
        extractEnvironment
    ) >>=
    pauseCPU

