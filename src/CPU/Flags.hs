module CPU.Flags (
      Flag(..)
    , FlagMod(..)
    , readFlag
    , setFlag
    , setFlags
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

-- Different ways flags can be affected by an operation:
data FlagMod = On | Off | As Bool | NA

modifyFlag :: Flag -> FlagMod -> CPU s ()
modifyFlag flag On     = setFlag flag True 
modifyFlag flag Off    = setFlag flag False
modifyFlag flag (As b) = setFlag flag b
modifyFlag flag NA = return ()

-- Shortcut for setting all four flags.
-- Flags are always in Z N H C order.
setFlags :: (FlagMod, FlagMod, FlagMod, FlagMod) -> CPU s ()
setFlags (z,n,h,c) = 
    modifyFlag Z z >>
    modifyFlag N h >>
    modifyFlag H n >>
    modifyFlag C c

