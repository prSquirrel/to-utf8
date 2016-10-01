module Encoder where

import Data.Char
import Data.Bits


-- Pattern: 110x xxxx 10xx xxxx
--
--     AND: 1101 1111 1011 1111 (0xDFBF)
--      OR: 1100 0000 1000 0000 (0xC080)

encodeUtf8 :: Int -> Int
encodeUtf8 x
            | x < 128 = x
            | otherwise = (x .&. 0xDFBF) .|. 0xC080