module EncoderSpec (spec) where

import           Encoder
import           Test.Hspec
import           Control.Monad
import           Control.Applicative
import           Control.Arrow

spec :: Spec
spec =
  describe "dec2hex" $
    context "given a decimal number" $
      do
        let assert rules =
              forM_ rules (\(input, expectation) -> dec2hex input `shouldBe` expectation)
        context "from 0 to 9" $
          it "converts it to hex byte directly" $
            do
              let rules =
                    map (second Hex)
                      [ (0, ["0x0"])
                      , (1, ["0x1"])
                      , (2, ["0x2"])
                      , (3, ["0x3"])
                      , (4, ["0x4"])
                      , (5, ["0x5"])
                      , (6, ["0x6"])
                      , (7, ["0x7"])
                      , (8, ["0x8"])
                      , (9, ["0x9"])
                      ]
              assert rules
        context "from 10 to 15" $
          it "converts it to hexadecimal letters" $
            do
              let rules =
                    map (second Hex)
                      [ (10, ["0xA"])
                      , (11, ["0xB"])
                      , (12, ["0xC"])
                      , (13, ["0xD"])
                      , (14, ["0xE"])
                      , (15, ["0xF"])
                      ]
              assert rules