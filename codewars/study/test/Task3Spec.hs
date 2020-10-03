module Task3Spec (spec) where

import Test.Hspec
import Test.QuickCheck

import Task3

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
spec :: Spec
spec = do
    describe "playDigits" $ do
        it "1st series" $ do
            digpow 89 1 `shouldBe` 1
            digpow 92 1 `shouldBe` -1
            digpow 46288 3 `shouldBe` 51
