module EncoderSpec (spec) where

import           Encoder
import           Test.Hspec
import           Control.Monad
import           Control.Applicative
import           Control.Arrow

spec :: Spec
spec =
  describe "Encoder" $ do
    context "given a decimal number" $
      do
        let assert rules =
              forM_ rules (\(input, expectation) -> dec2hex input `shouldBe` expectation)
        context "from 0 to 9" $
          it "converts it to hex byte directly" $
            do
              let rules =
                    map (second Hex)
                      [ (0, ['0'])
                      , (1, ['1'])
                      , (2, ['2'])
                      , (3, ['3'])
                      , (4, ['4'])
                      , (5, ['5'])
                      , (6, ['6'])
                      , (7, ['7'])
                      , (8, ['8'])
                      , (9, ['9'])
                      ]
              assert rules
        context "from 10 to 15" $
          it "converts it to hexadecimal letters" $
            do
              let rules =
                    map (second Hex)
                      [(10, ['A']), (11, ['B']), (12, ['C']), (13, ['D']), (14, ['E']), (15, ['F'])]
              assert rules
    context "given 16" $
      it "converts it to 10" $ do
        show (dec2hex 16) `shouldBe` "0x10"
    context "given hex bytes [A, F, 8, 2, 2]" $
      it "shows in reverse order starting with 0x" $
        show (Hex ['A', 'F', '8', '2', '2']) `shouldBe` "0x282AF"