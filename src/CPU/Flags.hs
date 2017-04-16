module CPU.Flags (
      Flag(..)
    , readFlag
    , setFlag
    , carriedAdd
    ) where
--
-- The f register also acts as the flags.

import CPU
import CPU.Environment
import Data.Bits

data Flag = Z | N | H | C

flagBit :: Flag -> Int
flagBit f = case f of
    Z -> 7
    N -> 6
    H -> 5
    C -> 4

readFlag :: Flag -> CPU s Bool
readFlag flag = do
    flags <- readReg f
    return $ testBit flags $ flagBit flag

setFlag :: Flag -> Bool -> CPU s ()
setFlag flag b = do
    flags <- readReg f
    let newFlags = setBit flags $ flagBit flag
    writeReg f newFlags

