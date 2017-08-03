{-# LANGUAGE Rank2Types #-}

module Main where

import CPU
import CPURunner
import CPU.Environment
import CPU.FrozenEnvironment
import CPU.Instructions
import CPU.Types
import Cartridge
import Numeric (readHex)
import Data.Bits (testBit)

import Control.Monad.ST
import Data.Array
import Data.Word
import Text.Printf
import System.IO

viewCPU :: FrozenCPUEnvironment -> String
viewCPU cpu = 
            "A F  B C  D E  H L   SP \n" ++
    (printf "%02X%02X %02X%02X %02X%02X %02X%02X %04X\n\n" 
        (frz_a cpu) (frz_f cpu) (frz_b cpu) (frz_c cpu) (frz_d cpu) (frz_e cpu) (frz_h cpu) (frz_l cpu) (frz_sp cpu)) ++
    (viewFlags $ frz_f cpu) ++
    "\n" ++
    (printf "PC = 0x%04X (0x%02X)\n" (frz_pc cpu) (frz_rom00 cpu ! (frz_pc cpu)))

viewFlags :: Word8 -> String
viewFlags f = 
    "Flags ZNHC \n" ++ 
    "      " ++ (b 7) ++ (b 6) ++ (b 5) ++ (b 4) ++ "\n"
    where 
        b n = if (testBit f n) then "1" else "0"

stepper :: FrozenCPUEnvironment -> Cycles -> IO ()
stepper cpuState cycles = do
    putStrLn (show cycles ++ " cycles elapsed\n")
    putStrLn $ viewCPU cpuState
    putStrLn "Press enter to step, or type 'QUIT' or 'MEM' > "
    command <- getLine
    case command of
        "" -> doStepFunc runStep
        "QUIT" -> putStrLn "Byee"
        "MEM"  -> putStrLn "Address: " >> readLn >>= printMem cpuState >> retry
        "RUN_TO" -> putStrLn "PC: " >> readLn >>= runToPc 
        "DUMP_VRAM" -> dumpVRAM cpuState >> retry
        _ -> putStrLn "Unknown command" >> retry
    
    where
        retry = stepper cpuState cycles
        printMem cpu addr = putStrLn $ printf "[0x%02X] 0x%02X" addr (frz_rom00 cpu ! addr)
        runToPc breakpoint = doStepFunc $ runCpu $ stepWhile (testPc (/= breakpoint))
        doStepFunc f =   
            let (nextState, extraCycles) = f cpuState
            in stepper nextState (cycles + extraCycles)

testPc :: (Register16 -> Bool) -> (CPU s Bool)
testPc f = readReg pc >>= return.f

type StopCondition s = CPU s Bool
type CycleCountedComputation s = CPU s Int

stepWhile :: forall s. StopCondition s -> CycleCountedComputation s
stepWhile condition = 
    stepWhile' condition 0
    where 
        stepWhile' condition sum = do
            continue <- condition
            if continue then
                step >>= \cycles -> stepWhile' condition (sum + cycles)
            else
                return sum

runCpu :: (forall s. CycleCountedComputation s) -> FrozenCPUEnvironment -> (FrozenCPUEnvironment, Cycles)
runCpu computation initialEnv = 
    runST $ resumeCPU initialEnv >>= runCPU (
        do
            cycles <- computation
            cpu <- extractEnvironment
            return (cpu, cycles)
    ) >>= pause

    where 
        pause (cpu,cycles) = do
            frozenCPU <- pauseCPU cpu
            return (frozenCPU, cycles)

runStep :: FrozenCPUEnvironment -> (FrozenCPUEnvironment, Cycles)
runStep = runCpu step

dumpVRAM :: FrozenCPUEnvironment -> IO ()
dumpVRAM  env = do
    let contents = foldl concattostring "" (frz_vram env) 
    writeFile "vram.txt" contents

    where concattostring s v = s ++ (show v)

main :: IO ()
main = do
    rom <- romFromFile
    let cpuState = defaultCPU { frz_rom00 = rom }
    stepper cpuState 0
