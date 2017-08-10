module Cartridge (
  romFromFile
  ) where

import Data.Array
import qualified Data.ByteString as BS 

import CPU.Types

romFromFile :: IO Memory
romFromFile = 
    fmap romFromByteString $ BS.readFile "Tetris.gb"

romFromByteString :: BS.ByteString -> Memory
romFromByteString contents = 
    array (0x00, 0x3FFF) $
    zip [0x00..0x3FFF] (take 0x4000 $ BS.unpack contents)

