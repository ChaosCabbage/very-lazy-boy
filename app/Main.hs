module Main where

import CPU
import Control.Monad.ST
import Data.Array
import Data.Word

main :: IO ()
main = putStrLn $ runST $ (initCPU rom) >>= runCPU test

rom :: Array Word16 Word8
rom = array (0x00, 0x3FFF) [(i, 0) | i <- [0x00..0x3FFF]]

-- Rom:  (0x00, 0x3FFF) 0x00

test :: CPU s String
test = do
    c1 <- execute 0x00
    c2 <- execute 0xC3
    pc <- readPC
    return $ "Total cycles: " ++ (show (c1 + c2)) ++ ", PC = " ++ (show pc)
