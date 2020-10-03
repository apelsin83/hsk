module Task2Spec (spec) where

import Test.Hspec
import Test.QuickCheck

import Task2


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Sample tests" $ do
    "world" `shouldBe` "world"