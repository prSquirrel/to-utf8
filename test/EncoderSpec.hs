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

gen16Bits :: Gen Int
gen16Bits = choose (0x800, 0xFFFF)

spec :: Spec
spec =
  describe "encodeUtf8" $ do
    it "has direct mapping for codes [0, 127]" $ quickCheckWith stdArgs { maxSuccess = 400 } $ property $
      forAll gen7Bits $ \x -> encodeUtf8 x == x

    it "adheres to pattern 0xxx xxxx for codes [0, 127]" $ quickCheckWith stdArgs { maxSuccess = 400 } $ property $ do
      -- 1000 0000
      let firstBit x = x .&. 0x80
      forAll gen7Bits $ \x -> (firstBit . encodeUtf8) x == 0

    it "adheres to pattern 110xx xxx 10xx xxxx for codes [128, 2047]" $ quickCheckWith stdArgs { maxSuccess = 4000 } $ property $ do
      -- 1110 0000 1100 0000
      let bitMask x = x .&. 0xE0C0
      -- 110x xxxx 10xx xxxx
      let pattern = 0xC080
      forAll gen11Bits $ \x -> (bitMask . encodeUtf8) x == pattern

    it "adheres to pattern 1110 xxxx 10xx xxxx 10xx xxxx for codes [2048, 65535]" $ quickCheckWith stdArgs { maxSuccess = 120000 } $ property $ do
      -- 1111 0000 1100 0000 1100 0000
      let bitMask x = x .&. 0xF0C0C0
      -- 1110 xxxx 10xx xxxx 10xx xxxx
      let pattern = 0xE08080
      forAll gen16Bits $ \x -> (bitMask . encodeUtf8) x == pattern