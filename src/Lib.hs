module Lib
    ( someFunc
    ) where

import Encoder
import Data.Char

someFunc :: IO ()
someFunc = do
    line <- getLine
    let number = read line :: Int
--     let utf = encodeUtf8 number
    undefined
--     putChar $ chr utf