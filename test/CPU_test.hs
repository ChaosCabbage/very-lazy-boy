module Main where

import CPU
import CPU.Instructions
import Rom
import CPUTestrunner
import Cartridge

import Data.Ix
import Data.Word
import Data.Array

main :: IO ()
main = do
    rom <- romFromFile
    putStrLn $ runCPUTest test rom 

tetris :: Array Word16 Word8
tetris = array (0x00, 0x3FFF) $
    (zerosBetween 0x00 0xFF) ++
    [(0x100, 0x00),
     (0x101, 0xC3),
     (0x102, 0xAB)] ++
    (zerosBetween 0x103 0x3FFF)

zerosBetween :: (Enum a, Num v) => a -> a -> [(a, v)]
zerosBetween n m = [(i, 0) | i <- [n..m]]

nSteps :: Int -> (CPU s Int)
nSteps n = nSteps' n 0 
    where nSteps' 0 sum = return sum
          nSteps' n sum = step >>= \c -> 
                          nSteps' (n-1) (sum + c)

test :: CPU s String
test = do
    cycles <- nSteps 4
    pc <- readPC
    return $ "Total cycles: " ++ (show cycles) ++ ", PC = " ++ (show pc)
