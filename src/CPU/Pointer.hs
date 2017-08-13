{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module CPU.Pointer (
    CPUPointer
  , pointer
  , pointerReg
  , pointerMinus
  , pointerPlus
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

-- Instance to automatically dereference the pointer.
instance CPUReference s (CPUPointer s) Word8 where
    readWord (Pointer address) = 
        address >>= readMemory

    writeWord (Pointer address) word = 
        address >>= \a -> writeMemory a word 
