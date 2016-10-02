module Encoder where

import           Data.Bits

encodeUtf8 :: Int -> Int
encodeUtf8 x
    | x <= 0x7F = x
    | x <= 0x7FF = 0xC080
          .|. maskByte1 x
          .|. maskByte2 x `shiftL` 8
    | x <= 0xFFFF = 0xE08080
          .|. maskByte1 x
          .|. maskByte2 x `shiftL` 8
          .|. maskByte3 x `shiftL` 16
    | x <= 0x10FFFF = 0xF0808080
          .|. maskByte1 x
          .|. maskByte2 x `shiftL` 8
          .|. maskByte3 x `shiftL` 16
          .|. maskByte4 x `shiftL` 24
  where
    maskByte1 x = x .&. 0x3F
    maskByte2 x = maskByte1 $ x `shiftR` 6
    maskByte3 x = maskByte1 $ x `shiftR` 12
    maskByte4 x = x `shiftR` 18