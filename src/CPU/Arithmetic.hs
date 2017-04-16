module Arithmetic (
    carriedAdd
    ) where

import Data.Word

type Carry = Bool

carriedAdd :: (Integral w, Bounded w) => w -> w -> (w, Carry)
carriedAdd x y = 
    let realAnswer = (fromIntegral x :: Word32) + (fromIntegral y :: Word32)
    in (fromIntegral realAnswer :: w, realAnswer > (maxBound :: w))

