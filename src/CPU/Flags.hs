module CPU.Flags (
      Flag(..)
    , FlagMod(..)
    , readFlag
    , setFlag
    , setFlagM
    , setFlags
    , modifyFlag
    ) where
--
-- The f register also acts as the flags.

import CPU
import CPU.Environment
import Data.Bits
import CPU.Reference

data Flag = Zf | Nf | Hf | Cf

flagBit :: Flag -> Int
flagBit flag = case flag of
    Zf -> 7
    Nf -> 6
    Hf -> 5
    Cf -> 4

readFlag :: Flag -> CPU s Bool
readFlag flag = do
    flags <- readWord F
    return $ testBit flags $ flagBit flag

setFlag :: Flag -> Bool -> CPU s ()
setFlag flag b = 
    modifyWord F changeBit
    where 
        op = if b then setBit else clearBit
        changeBit w = op w $ flagBit flag 

-- Different ways flags can be affected by an operation:
data FlagMod = On | Off | As Bool | NA

modifyFlag :: Flag -> FlagMod -> CPU s ()
modifyFlag flag On     = setFlag flag True 
modifyFlag flag Off    = setFlag flag False
modifyFlag flag (As bool) = setFlag flag bool
modifyFlag _    NA = return ()

-- Shortcut for setting all four flags.
-- Flags are always in Z N H C order.
setFlags :: (FlagMod, FlagMod, FlagMod, FlagMod) -> CPU s ()
setFlags (z,n,h,c) = 
    modifyFlag Zf z >>
    modifyFlag Nf n >>
    modifyFlag Hf h >>
    modifyFlag Cf c

setFlagM :: Flag -> CPU s Bool -> CPU s ()
setFlagM flag val = 
    val >>= (setFlag flag)