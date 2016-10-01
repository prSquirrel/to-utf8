module Encoder where

import           Data.Bits

encodeUtf8 :: Int -> Int
encodeUtf8 x
    | x <= 0x7F = x
    | x <= 0x7FF = 0xC080
          .|. ((x `shiftR` 6) `shiftL` 8)
          .|. (x .&. 0x3F)
    | x <= 0xFFFF = 0xE08080
          .|. (x .&. 0x3F)
          .|. (((x `shiftR` 6) .&. 0x3F) `shiftL` 8)
          .|. ((x `shiftR` 12) `shiftL` 16)
    | x <= 0x10FFFF = 0xF0808080
          .|. (x .&. 0x3F)
          .|. (((x `shiftR` 6) .&. 0x3F) `shiftL` 8)
          .|. (((x `shiftR` 12) .&. 0x3F) `shiftL` 16)
          .|. ((x `shiftR` 18) `shiftL` 24)