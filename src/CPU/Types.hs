module CPU.Types (
    Register8
  , Register16
  , Address
  , Opcode
  , Memory
    ) where

import Data.Word
import Data.Array

type Register8 = Word8
type Register16 = Word16
type Address = Word16
type Opcode = Word8
type Memory = Array Word16 Word8
