module Cartridge (
  romFromFile
  ) where

import Data.Array
import Data.Word (Word16)
import Data.List.Split (chunksOf)
import qualified Data.ByteString as BS 

import CPU.Types

romFromFile :: IO [Memory]
romFromFile = 
    fmap romFromByteString $ BS.readFile "Tetris.gb"

romFromByteString :: BS.ByteString -> [Memory]
romFromByteString contents = 
    let 
        bankSize = 0x4000 
        bankChunks = chunksOf 0x4000 $ BS.unpack contents
        bottom i = bankSize * (fromIntegral i) :: Word16
        top i = (bottom (i + 1)) - 1 
        bank i = array (bottom i, top i) $
                 zip [(bottom i)..(top i)] (bankChunks !! i)
    in
        [bank 0, bank 1] 
        -- This should really be dependent on the cartidge type.
        -- Some cartidges just have two banks. Others have more.

