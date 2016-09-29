module Encoder where

import           Data.Maybe (fromJust)
import           Data.Tuple (swap)

data Hex = Hex [String]
  deriving (Eq, Show)

instance Enum Hex where
  toEnum = fromJust . flip lookup table
  fromEnum = fromJust . flip lookup (map swap table)

table =
  zip [0 .. 15] [ Hex ["0x" ++ [c]]
                | c <- ['0' .. '9'] ++ ['A' .. 'F'] ]

dec2hex :: Int -> Hex
dec2hex x = toEnum x