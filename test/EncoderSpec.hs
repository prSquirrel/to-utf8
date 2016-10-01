module EncoderSpec ( spec ) where

import           Encoder
import           Test.Hspec
import           Test.QuickCheck hiding ( (.&.) )

import           Data.Bits

gen7Bits :: Gen Int
gen7Bits = choose (0, 0x7F)

gen11Bits :: Gen Int
gen11Bits = choose (0x80, 0x7FF)

gen16Bits :: Gen Int
gen16Bits = choose (0x800, 0xFFFF)

gen21Bits :: Gen Int
gen21Bits = choose (0x10000, 0x10FFFF)

spec :: Spec
spec = describe "encodeUtf8" $ do
    it "has direct mapping for codes [0, 127]" $
        quickCheckWith stdArgs { maxSuccess = 200 } $
            property $
                forAll gen7Bits $ \x -> encodeUtf8 x == x
    describe "adheres to pattern" $ do
        it "0xxx xxxx for codes [0, 127]" $
            quickCheckWith stdArgs { maxSuccess = 200 } $
                property $ do
                    let firstBit x = x .&. 0x80 -- 1000 0000
                    forAll gen7Bits $ \x -> (firstBit . encodeUtf8) x == 0

        it "110x xxxx 10xx xxxx for codes [128, 2047]" $
            quickCheckWith stdArgs { maxSuccess = 2000 } $
                property $ do
                    let bitMask x = x .&. 0xE0C0 -- 1110 0000 1100 0000
                    forAll gen11Bits $ \x -> (bitMask . encodeUtf8) x == 0xC080 -- 1100 0000 1000 0000

        it "1110 xxxx 10xx xxxx 10xx xxxx for codes [2048, 65535]" $
            quickCheckWith stdArgs { maxSuccess = 64000 } $
                property $ do
                    let bitMask x = x .&. 0xF0C0C0 -- 1111 0000 1100 0000 1100 0000
                    forAll gen16Bits $
                        \x -> (bitMask . encodeUtf8) x == 0xE08080 -- 1110 0000 1000 0000 1000 0000

        it "1111 0xxx 10xx xxxx 10xx xxxx 10xx xxxx for codes [65536, 1114111]" $
            quickCheckWith stdArgs { maxSuccess = 1050000 } $
                property $ do
                    let bitMask x = x .&. 0xF8C0C0C0 -- 1111 1000 1100 0000 1100 0000 1100 0000
                    forAll gen21Bits $
                        \x -> (bitMask . encodeUtf8) x == 0xF0808080 -- 1111 0000 1000 0000 1000 0000 1000 0000