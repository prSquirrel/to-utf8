module EncoderSpec (spec) where

import           Encoder
import           Test.Hspec
import Test.QuickCheck hiding ((.&.))

import Data.Char
import Data.Bits

gen7Bits :: Gen Int
gen7Bits = choose (0, 0x7F)

gen11Bits :: Gen Int
gen11Bits = choose (0x80, 0x7FF)

spec :: Spec
spec =
  describe "encodeUtf8" $ do
    it "has direct mapping for codes [0, 127]" $ quickCheckWith stdArgs { maxSuccess = 1000 } $ property $
      forAll gen7Bits $ \x -> encodeUtf8 x == x
    it "adheres to pattern 0xxxxxxx for codes [0, 127]" $ quickCheckWith stdArgs { maxSuccess = 1000 } $ property $ do
      let firstBit x = x .&. 0x80 -- mask: 1000 0000
      forAll gen7Bits $ \x -> (firstBit . encodeUtf8) x == 0
    it "adheres to pattern 110xxxxx 10xxxxxx for codes [128, 2047]" $ quickCheckWith stdArgs { maxSuccess = 10000 } $ property $ do
      let bitMask x = x .&. 0xE0C0 -- mask: 1110 0000 1100 0000
      let pattern = 0xC080 -- 110x xxxx 10xx xxxx
      forAll gen11Bits $ \x -> (bitMask . encodeUtf8) x == pattern
