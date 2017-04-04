module Cartridge (
  romFromFile
  ) where

import Data.Word
import Data.Array
import qualified Data.ByteString as BS 

import Rom

romFromFile :: IO Rom
romFromFile = 
    fmap romFromByteString $ BS.readFile "Tetris.gb"

romFromByteString :: BS.ByteString -> Rom
romFromByteString contents = 
    array (0x00, 0x3FFF) $
    zip [0x00..0x3FFF] (take 0x4000 $ BS.unpack contents)

