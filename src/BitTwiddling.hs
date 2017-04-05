module BitTwiddling (
    joinBytesM
    ) where

import Data.Word
import Data.Bits
import Control.Monad

to16 :: Word8 -> Word16
to16 = fromIntegral 

joinBytes :: Word8 -> Word8 -> Word16
joinBytes low high = 
    let low16 = to16 low;
        high16 = shiftL (to16 high) 8
    in
        high16 + low16

joinBytesM :: (Monad m) => m Word8 -> m Word8 -> m Word16
joinBytesM = liftM2 joinBytes
