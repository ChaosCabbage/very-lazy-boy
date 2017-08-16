module CPU.Types (
    Address
  , Opcode
  , Memory
  , Cycles
    ) where

import Data.Word
import Data.Array

type Address = Word16
type Opcode = Word8
type Memory = Array Word16 Word8

type Cycles = Int
