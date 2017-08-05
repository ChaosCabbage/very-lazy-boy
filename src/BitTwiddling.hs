module BitTwiddling (
    joinBytes
  , joinBytesM
  , to16
  , to8
  , toBytes
  , toSigned
    ) where

import Data.Word
import Data.Int
import Data.Bits
import Control.Monad

to16 :: Word8 -> Word16
to16 = fromIntegral 

to8 :: Word16 -> Word8
to8 = fromIntegral

toSigned :: Word8 -> Int8
toSigned = fromIntegral

toBytes :: Word16 -> (Word8, Word8)
toBytes w = (to8 w, to8 $ shiftR w 8)
        
joinBytes :: Word8 -> Word8 -> Word16
joinBytes low high = 
    let low16 = to16 low;
        high16 = shiftL (to16 high) 8
    in
        high16 + low16

joinBytesM :: (Monad m) => m Word8 -> m Word8 -> m Word16
joinBytesM = liftM2 joinBytes

