module Encoder where

import           Data.Bits
import           Data.Word

encodeUtf8 :: Word -> [Word8]
encodeUtf8 x
    | x <= 0x7F = [ fromIntegral x ]
    | x <= 0x7FF = applyPattern [ 0xC0, 0x80 ] x
    | x <= 0xFFFF = applyPattern [ 0xE0, 0x80, 0x80 ] x
    | x <= 0x10FFFF = applyPattern [ 0xF0, 0x80, 0x80, 0x80 ] x

applyPattern :: [Word8] -> Word -> [Word8]
applyPattern p x = reverse $
    zipWith ($) fns (chunkSixBytes x)
  where
    fns = map (.|.) (reverse p)

chunkSixBytes :: Word -> [Word8]
chunkSixBytes 0 = [ 0 ]
chunkSixBytes x = fromIntegral (x .&. 0x3F) : chunkSixBytes (x `shiftR` 6)