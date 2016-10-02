module EncoderSpec ( spec ) where

import           Encoder
import           Test.Hspec
import           Test.QuickCheck hiding ( (.&.) )

import           Data.Bits
import           Data.Word

gen7Bits :: Gen Word
gen7Bits = choose (0, 0x7F)

gen11Bits :: Gen Word
gen11Bits = choose (0x80, 0x7FF)

gen16Bits :: Gen Word
gen16Bits = choose (0x800, 0xFFFF)

gen21Bits :: Gen Word
gen21Bits = choose (0x10000, 0x10FFFF)

spec :: Spec
spec = describe "encodeUtf8" $ do
    it "has direct mapping for codes [0, 127]" $
        quickCheckWith stdArgs { maxSuccess = 200 } $
            property $
                forAll gen7Bits $
                    \x -> encodeUtf8 x == [ fromIntegral x :: Word8 ]
    describe "adheres to pattern" $ do
        it "0xxx xxxx for codes [0, 127]" $
            quickCheckWith stdArgs { maxSuccess = 200 } $
                property $ do
                    let firstBit = (.&.) 0x80
                    forAll gen7Bits $ \x -> map firstBit (encodeUtf8 x) == [ 0 ]

        it "110x xxxx 10xx xxxx for codes [128, 2047]" $
            quickCheckWith stdArgs { maxSuccess = 2000 } $
                property $ do
                    let bitMask = map (.&.) [ 0xE0, 0xC0 ]
                    forAll gen11Bits $
                        \x -> zipWith ($) bitMask (encodeUtf8 x) ==
                            [ 0xC0, 0x80 ]

        it "1110 xxxx 10xx xxxx 10xx xxxx for codes [2048, 65535]" $
            quickCheckWith stdArgs { maxSuccess = 10000 } $
                property $ do
                    let bitMask = map (.&.) [ 0xF0, 0xC0, 0xC0 ]
                    forAll gen16Bits $
                        \x -> zipWith ($) bitMask (encodeUtf8 x) ==
                            [ 0xE0, 0x80, 0x80 ]

        it "1111 0xxx 10xx xxxx 10xx xxxx 10xx xxxx for codes [65536, 1114111]" $
            quickCheckWith stdArgs { maxSuccess = 50000 } $
                property $ do
                    let bitMask = map (.&.) [ 0xF8, 0xC0, 0xC0, 0xC0 ]
                    forAll gen21Bits $
                        \x -> zipWith ($) bitMask (encodeUtf8 x) ==
                            [ 0xF0, 0x80, 0x80, 0x80 ]

    describe "integration tests" $ do
        context "given 288" $
            it "encodes to C4 A0" $
                encodeUtf8 288 `shouldBe` [ 0xC4, 0xA0 ]
        context "given 2336" $
            it "encodes to E0 A4 A0" $
                encodeUtf8 2336 `shouldBe` [ 0xE0, 0xA4, 0xA0 ]
        context "given 65536" $
            it "encodes to F0 90 80 80" $
                encodeUtf8 65536 `shouldBe` [ 0xF0, 0x90, 0x80, 0x80 ]
        context "given 67102" $
            it "encodes to F0 90 98 9E" $
                encodeUtf8 67102 `shouldBe` [ 0xF0, 0x90, 0x98, 0x9E ]
        context "given 66376" $
            it "encodes to F0 90 8D 88" $
                encodeUtf8 66376 `shouldBe` [ 0xF0, 0x90, 0x8D, 0x88 ]