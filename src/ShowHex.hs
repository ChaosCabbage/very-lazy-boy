module ShowHex (
    showHex
    ) where

import qualified Numeric (showHex)
import Data.Char

showHex :: (Show a, Integral a) => a -> String
showHex x = "0x" ++ (map toUpper $ Numeric.showHex x "")
