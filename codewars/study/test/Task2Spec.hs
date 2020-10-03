module Task2Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Task2

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "should reduce duplicates" $ do
    uniqueInOrder "AAAABBBCCDAABBB" `shouldBe` "ABCDAB"
    uniqueInOrder "A" `shouldBe` "A"
    uniqueInOrder "" `shouldBe` ""

