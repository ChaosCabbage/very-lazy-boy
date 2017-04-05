module CPU.Instructions (
    execute
  , step
    ) where

import CPU.Types
import CPU.Environment
import CPU

import Data.Char
import Numeric (showHex)

type Cycles = Int

step :: CPU s Cycles
step = fetch >>= execute

execute :: Opcode -> CPU s Cycles
execute 0x00 = nop
execute 0xC3 = fetch16 >>= jp
-- execute 0xAF = set A to zero
execute x = error $ "Unknown opcode 0x" ++ (map toUpper (showHex x ""))

nop :: CPU s Cycles
nop = return 4

jp :: Address -> CPU s Cycles
jp addr = 
    writePC addr >> 
    return 16