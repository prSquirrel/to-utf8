module Encoder where

import           Data.Bits

-- Pattern: 110x xxxx 10xx xxxx
--
--     AND: 1101 1111 1011 1111 (0xDFBF)
--      OR: 1100 0000 1000 0000 (0xC080)
--
-- Pattern: 1110 xxxx 10xx xxxx 10xx xxxx
--
--     AND: 1110 1111 1011 1111 1011 1111 (0xEFBFBF)
--      OR: 1110 0000 1000 0000 1000 0000 (0xE08080)
--
-- Pattern: 1111 0xxx 10xx xxxx 10xx xxxx 10xx xxxx
--
--     AND: 1111 0111 1011 1111 1011 1111 1011 1111 (0xF7BFBFBF)
--      OR: 1111 0000 1000 0000 1000 0000 1000 0000 (0xF0808080)
--
encodeUtf8 :: Int -> Int
encodeUtf8 x
    | x <= 0x7F = x
    | x <= 0x7FF = (x .&. 0xDFBF) .|. 0xC080
    | x <= 0xFFFF = (x .&. 0xEFBFBF) .|. 0xE08080
    | x <= 0x10FFFF = (x .&. 0xF7BFBFBF) .|. 0xF0808080