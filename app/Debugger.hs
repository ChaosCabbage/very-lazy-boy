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
        runToPc pc = doStepFunc $ stepWhile (\env -> frz_pc env /= pc)
        doStepFunc f =   
            let (nextState, extraCycles) = f cpuState
            in stepper nextState (cycles + extraCycles)

-- Probably going to be slow, since it's dipping in and out of the ST each step.
-- To do: figure out how to do this inside the monad. 
stepWhile :: (FrozenCPUEnvironment -> Bool) -> FrozenCPUEnvironment -> (FrozenCPUEnvironment, Cycles)
stepWhile condition env = stepWhile' condition 0 env 
    where stepWhile' condition sum env = 
            if 
                condition env 
            then 
                let stepResult = runStep env
                in stepWhile' condition (sum + snd stepResult) (fst stepResult) 
            else 
                (env, sum)

testPc :: (Register16 -> Bool) -> (CPU s Bool)
testPc f = readReg pc >>= return.f 

runStep :: FrozenCPUEnvironment -> (FrozenCPUEnvironment, Cycles)
runStep initialEnv = 
    runST $ resumeCPU initialEnv >>= runCPU (
        do
            cycles <- step
            cpu <- extractEnvironment
            return (cpu, cycles)
    ) >>= pause

    -- I wonder if there's a built in function to do this.
    -- (m a, c) -> (m a -> m b) -> m (b, c)
    where 
        pause (cpu,cycles) = do
            frozenCPU <- pauseCPU cpu
            return (frozenCPU, cycles)

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
