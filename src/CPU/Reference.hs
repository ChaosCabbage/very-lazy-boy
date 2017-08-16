{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module CPU.Reference (
    CPUReference(..)
) where
    
-- A bit of an experiment.
-- There's a lot of repeated code because the types of various registers are different.
-- Can I unify it?

import CPU
import CPU.Environment (Register8, Register16, ComboRegister)
import Data.Bits (FiniteBits)
import Data.Word (Word8, Word16)
    
class (Num w, Bounded w, FiniteBits w, Integral w) => CPUReference s a w | a -> w where
    readWord :: a -> CPU s w
    writeWord :: a -> w -> CPU s ()
    modifyWord :: a -> (w -> w) -> CPU s ()
    modifyWord ref transform = 
        (readWord ref) >>= (writeWord ref) . transform

instance CPUReference s Register8 Word8 where
    readWord = readReg8
    writeWord = writeReg8
    
instance CPUReference s Register16 Word16 where
    readWord = readReg16
    writeWord = writeReg16

instance CPUReference s ComboRegister Word16 where
    readWord = readComboReg
    writeWord = writeComboReg
