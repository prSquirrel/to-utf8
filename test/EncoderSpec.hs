module EncoderSpec (spec) where

import           Encoder
import           Test.Hspec
import           Control.Monad
import           Control.Applicative
import           Control.Arrow

spec :: Spec
spec =
  describe "Encoder" $
    it "should work" $
      1 + 1 `shouldBe` 2