module Encoder where

import           Data.Maybe (fromJust)
import           Data.Tuple (swap)
import Data.Monoid ((<>))
import Data.List


dec2hex :: Int -> Hex
dec2hex x
  | x < 16 = toEnum x
  | x < 256 = dec2hex (x `div` 16) `mappend` dec2hex (x `mod` 16)


data Hex = Hex [Char]
  deriving (Eq)

instance Enum Hex where
  toEnum = fromJust . flip lookup table
  fromEnum = fromJust . flip lookup (map swap table)

table =
  zip [0 .. 15] [ Hex [c]
                | c <- ['0' .. '9'] ++ ['A' .. 'F'] ]

instance Show Hex where
    show (Hex xs) = ("0x" ++) . join . reverse . chunks 2 $ xs

instance Monoid Hex where
    (Hex x) `mappend` (Hex y) = Hex (x ++ y)
    mempty = Hex []

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

join :: [String] -> String
join = intercalate ""