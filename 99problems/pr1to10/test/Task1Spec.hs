module Task1Spec (spec) where

import Test.Hspec
import Test.QuickCheck

import Task1


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Sample tests" $ do
    myLast "world" `shouldBe` 'd'