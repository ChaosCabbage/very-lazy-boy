module CPU.IORegisters (
    IORegisters,
    FrozenIORegisters,
    create,
    thaw,
    freeze,
    addCycles,
    readPort,
    writePort
) where

import CPU.Types (Address)
import Data.Word (Word8)
import Data.STRef (STRef(..), readSTRef, writeSTRef)
import Control.Monad.ST
import Text.Printf (printf)

-- IO registers act like normal memory locations from the point of view
-- of most gameboy instructions.
-- However, they are not real memory.
-- Reading from and writing to these addresses is how gameboy programs
-- interact with the hardware.
-- 
-- This structure will keep all of the actual state needed to keep track
-- of the hardware.
-- Right now, that just means the number of cycles elapsed.
data IORegisters s = IORegisters {
    elapsedCycles :: STRef s Int
}
--
-- Let's have a handy access function for the number of cycles
cycles :: IORegisters s -> ST s Int
cycles io = readSTRef (elapsedCycles io)
--
-- And I need a frozen version for outside of the CPU monad.
data FrozenIORegisters = FrozenIORegisters {
    frz_elapsedCycles :: Int
}
-- (Maybe I can generate this pair with some template haskell?)
--
----- Construction
--
-- Of course, we start with zero cycles!
create :: FrozenIORegisters
create = FrozenIORegisters { frz_elapsedCycles = 0 }
--
thaw :: FrozenIORegisters -> ST s (IORegisters s)
thaw frzio =
    newSTRef (frz_elapsedCycles frzio) >>=
    return . IORegisters
--
freeze :: IORegisters s -> ST s FrozenIORegisters
freeze io =
    cycles io >>= 
    return . FrozenIORegisters
--
----- Updates
--
-- We need to keep track of extra cycles whenver we execute instructions
addCycles :: IORegisters s -> Int -> ST s ()
addCycles io newCycles = do
    current <- cycles io
    writeSTRef (elapsedCycles io) (current + newCycles) 
--
----- Access
--
-- Here's our main memory reading function.
-- Eventually, this will be the whole IO map.
-- Right now I've just the LCD Y register.
readPort :: Address -> IORegisters s -> ST s Word8 
readPort addr io
    | isIOPort addr =
        case addr of
            0xFF44 -> lcdYCoordinate io
            _      -> return 0x00 -- Default placeholder until all ports implemented
    | otherwise = 
        error $ printf "Address 0x%04X is not an IO port" addr  
--
-- And the corresponding map for writing...
-- Most IO registers are read-only. Some of them are reset to zero upon writing.
-- A few can genuinely be written to.
-- As a first implementation, it seemed safest to do nothing.
writePort :: Address -> IORegisters s -> ST s ()
writePort addr _ 
    | isIOPort addr = return ()
    | otherwise = 
        error $ printf "Address 0x%04X is not an IO port" addr   
--
-- IO Ports are the memory range from 0xFF00 through 0xFF7F
isIOPort :: Address -> Bool
isIOPort addr = 
    addr >= 0xFF00 && addr < 0xFF80
--
-- 0xFF44: The current scanline being written to by the LCD driver.
--   Scanlines from 144 to 153 indicate the vblank period.
lcdYCoordinate :: IORegisters s -> ST s Word8
lcdYCoordinate io = 
    cycles io >>= 
    return . fromIntegral . lcdScanline

-- The LCD refreshes the screen one line at a time,
-- like this:
--   Lines 0 - 143 each take 204 cycles
--   Lines 144 - 153 each take 456 cycles. This is the VBlank period. These lines can't be seen.
--   Finally, there are 252 cycles before line 0 starts again.
lcdScanline :: Int -> Int
lcdScanline elapsedCycles
    | modCycles <= hBlankCycles = modCycles `div` 20
    | modCycles <= totalCycles  = ((modCycles - hBlankCycles) `div` 456) + 143
    | otherwise                 = 0
    where
        hBlankCycles = 143 * 204
        vBlankCycles =  10 * 456
        otherCycles  = 252
        totalCycles  = hBlankCycles + vBlankCycles + otherCycles
        modCycles    = elapsedCycles `mod` totalCycles

