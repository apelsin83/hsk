module HOFTaskSpec (main, spec) where
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec
import Test.QuickCheck

import HOFLists

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Sample tests" $ do
    "world" `shouldBe` "world"