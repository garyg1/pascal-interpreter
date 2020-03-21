module Pascal.ScopeSpec (spec) where

import           Control.DeepSeq
import           Control.Exception (evaluate)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Pascal.Data       as D
import qualified Pascal.Scope      as Scope
import           Test.Hspec


spec :: Spec
spec = do
    describe "insert" $ do
        it "should throw if symbol is const" $ do
            let name = D.Id "test"
                scope = (Scope.insert name (1 :: Int) . Scope.setConst True name) Scope.empty
                value = Map.lookup name (Scope.vars scope)
            (evaluate . force) value `shouldThrow` anyException

        it "should insert value as VAR otherwise" $ do
            let name = D.Id "test"
                scope = Scope.insert name (1 :: Int) Scope.empty
                value = Map.lookup name (Scope.vars scope)
            value `shouldBe` Just 1

    describe "setConst" $ do
        it "should add to consts if true" $ do
            let name = D.Id "test"
                (Scope.Scope _ consts) = Scope.setConst True name Scope.empty
            Set.member name consts `shouldBe` True

        it "should remove from consts if false" $ do
            let name = D.Id "test"
                scope = Scope.Scope Map.empty (Set.fromList [name])
                (Scope.Scope _ consts) = Scope.setConst False name scope
            Set.member name consts `shouldBe` False

    describe "isConst" $ do
        it "should return Nothing if name not in Scope.vars" $ do
            let name = D.Id "test"
                scope = Scope.Scope Map.empty (Set.fromList [name])
            Scope.isConst name scope `shouldBe` Nothing

        it "should return True if name in Scope.vars and in Scope.constSymbols" $ do
            let name = D.Id "test"
                scope = Scope.Scope (Map.fromList [(name, 1::Int)]) (Set.fromList [name])
            Scope.isConst name scope `shouldBe` Just True

        it "should return False if name in Scope.vars and in Scope.constSymbols" $ do
            let name = D.Id "test"
                scope = Scope.Scope (Map.fromList [(name, 1::Int)]) Set.empty
            Scope.isConst name scope `shouldBe` Just False