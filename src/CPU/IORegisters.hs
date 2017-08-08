module CPU.IORegisters (
    IORegisters(..)
) where

lcdScanline :: Int -> Int
lcdScanline cycles = 
    | modcycles <= 29172 = modcycles `div` 204
    | modcycles <= 33732 = ((modcycles - 29172) `div` 456) + 143
    | otherwise          = 0
    where
        modcycles = cycles `mod` 33984


data IORegisters = IORegisters {
    elapsedCycles :: STRef s Int
}


