{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module CPU.Pointer (
    CPUPointer
  , pointer
  , pointerReg
  , pointerMinus
  , pointerPlus
  , readPtr
  , writeToPtr
  , modifyThroughPtr
) where

import CPU (CPU, readMemory, writeMemory)
import CPU.Types (Address)
import CPU.Reference (CPUReference(..))
import Data.Word (Word8, Word16)

-- Pointers. This is when a 16 bit piece of data points at an address.
-- Pointers are monadic to aid writing instructions where the pointer
-- is modified during dereference, e.g. LD (HL+),A

newtype CPUPointer s = Pointer (CPU s Address) 

pointer :: Address -> CPUPointer s
pointer addr = Pointer $ return addr

-- e.g. LD (HL) instructions
pointerReg :: (CPUReference s r Word16) => r -> CPUPointer s
pointerReg ref = Pointer $ readWord ref

-- e.g. LD (HL-) instructions
pointerMinus :: (CPUReference s r Word16) => r -> CPUPointer s
pointerMinus ref = Pointer $ do
    addr <- readWord ref
    modifyWord ref (subtract 1 :: Word16 -> Word16)
    return addr

-- e.g. LD (HL+) instructions
pointerPlus :: (CPUReference s p Word16) => p -> CPUPointer s
pointerPlus ref = Pointer $ do
    addr <- readWord ref
    modifyWord ref ((+1) :: Word16 -> Word16)
    return addr

readPtr :: CPUPointer s -> CPU s Word8
readPtr (Pointer address) = address >>= readMemory

writeToPtr :: CPUPointer s -> Word8 -> CPU s ()  
writeToPtr (Pointer address) word = 
        address >>= \a -> writeMemory a word 

modifyThroughPtr :: CPUPointer s -> (Word8 -> Word8) -> CPU s ()
modifyThroughPtr p f = readPtr p >>= (writeToPtr p) . f

{-
It would simplify a few things if I could do this:

instance CPUReference s (CPUPointer s) Word8 where
    readWord = readPtr
    writeWord = writeToPtr

HOWEVER
When you try to write a function

f :: (CPUReference s r Word8) -> r -> CPU s ()

You get an ambiguous type!
Since CPUPointer is parameterized on s, the r here is dependent on s,
but the compiler can't figure out that the CPUReference s and the CPUPointer s are the same s.
I've tried putting "forall s." everywhere, but I can't figure out how to do it.

-}
