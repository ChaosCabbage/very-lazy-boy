module Rom (
    Rom
  , emptyRom
    ) where

import Data.Array
import Data.Word

type Rom =  Array Word16 Word8

-- A rom which is a big old array of zeros.
emptyRom :: Rom
emptyRom = array 
    (0x00, 0x3FFF) 
    [(i, 0) | i <- [0x00..0x3FFF]]
