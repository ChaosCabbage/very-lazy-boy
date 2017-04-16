{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Arithmetic (
    carriedAdd
    ) where

import Data.Word

type Carry = Bool

as32 :: (Integral a) => a -> Word32 
as32 = fromIntegral


carriedAdd :: forall w. (Integral w, Bounded w) => w -> w -> (w, Carry)
carriedAdd x y = 
    let 
        max = as32 (maxBound :: w)
        realAnswer = (as32 x) + (as32 y)
    in 
        (fromIntegral realAnswer :: w, realAnswer > max)


