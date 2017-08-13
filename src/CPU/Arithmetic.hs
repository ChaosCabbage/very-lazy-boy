{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Arithmetic (
    carriedAdd
  , carriedSubtract
  , signedAdd
    ) where

import Data.Word
import Data.Bits (FiniteBits, finiteBitSize, shiftR, (.&.), testBit)
import BitTwiddling (toSigned)

type Carry = Bool
type HalfCarry = Bool 

topByte :: (Integral w, FiniteBits w) => w -> Word8
topByte word = 
    let bits = finiteBitSize word
        shifted = word `shiftR` (bits - 8)
    in 
        fromIntegral shifted :: Word8

-- Good explanation of the half-carry here :
--  http://robdor.com/2016/08/10/gameboy-emulator-half-carry-flag/
--
checkHalfCarry :: (FiniteBits w, Integral w) => w -> w -> HalfCarry 
checkHalfCarry x y =
    let xNybble = (topByte x) .&. 0x0F
        yNybble = (topByte y) .&. 0x0F
        nybbleSum = xNybble + yNybble
    in
        testBit nybbleSum 4
    

as32 :: (Integral a) => a -> Word32 
as32 = fromIntegral

carriedAdd :: forall w. (FiniteBits w, Integral w, Bounded w) => w -> w -> (w, Carry, HalfCarry)
carriedAdd x y = 
    let 
        maxInt = as32 (maxBound :: w)
        realAnswer = (as32 x) + (as32 y)
    in 
        (fromIntegral realAnswer :: w, realAnswer > maxInt, checkHalfCarry x y) 

-- I believe what happens is the (2's complement) negative of y
-- is added to x, and the flags set accordingly.  
carriedSubtract :: (FiniteBits w, Integral w, Bounded w) => w -> w -> (w, Carry, HalfCarry)
carriedSubtract x y = 
    carriedAdd x (-y)

-- Signed addition: 16-bit + signed 2's complement 8-bit
-- Convert the signed number to 16 bit and add.
-- Not entirely sure that this gets the flags right but it's fine for now.
signedAdd :: Word16 -> Word8 -> (Word16, Carry, HalfCarry)
signedAdd x y = 
    let signed = toSigned y
        w16 = fromIntegral signed :: Word16 
    in 
        carriedAdd x w16 
