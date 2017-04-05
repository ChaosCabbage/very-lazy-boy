module CPU.Types (
    Register8
  , Register16
  , Address
  , Opcode
    ) where

import Data.Word

type Register8 = Word8
type Register16 = Word16
type Address = Word16
type Opcode = Word8
