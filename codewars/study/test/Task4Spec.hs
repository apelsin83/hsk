module Task4Spec (spec) where

import Test.Hspec
import Test.QuickCheck

import Task2

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
spec :: Spec
spec = do
  describe "Example Test" $
    it "should work with example tests" $ do
      countPositivesSumNegatives (Just [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -11, -12, -13, -14, -15]) `shouldBe` [10, -65]
      countPositivesSumNegatives (Just [0, 2, 3, 0, 5, 6, 7, 8, 9, 10, -11, -12, -13, -14]) `shouldBe` [8, -50]