module Pascal.InterpretSpec (spec) where

import           Test.Hspec
-- import           Test.QuickCheck
import           Pascal.Interpret as I

spec :: Spec
spec = do
  describe "combineToBool" $ do
    it "works" $ do
        (I.combineToBool "<>" (1::Integer) 2) `shouldBe` True
