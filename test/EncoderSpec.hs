module EncoderSpec (spec) where

import           Encoder
import           Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "Encoder" $
    it "should work" $
      encodeUtf8 0 `shouldBe` '\0'