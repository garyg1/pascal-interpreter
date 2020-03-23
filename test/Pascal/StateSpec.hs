module Pascal.StateSpec (spec) where

import           Control.DeepSeq
import           Control.Exception     (evaluate)
import           Control.Monad.State
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Knob
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import qualified Data.Set              as Set
import qualified Pascal.Data           as D
import qualified Pascal.Scope          as Scope
import qualified Pascal.State          as S
import           Pascal.TestUtils      (extract, run, runWithStreams)
import           System.IO             (IOMode (WriteMode), hClose)
import qualified System.IO.Streams     as Streams
import           Test.Hspec

spec :: Spec
spec = do
    let name = D.Id "myVar"
        value = S.IntValue 1
        value' = S.IntValue 2
        value'' = S.IntValue 3

    let scopeFromList vars = Scope.Scope {
        Scope.vars = Map.fromList vars,
        Scope.constSymbols = Set.empty}

    let canaryScope = scopeFromList [(name, value'')]
        replaceWithCanary _ = canaryScope

    describe "declare" $ do
        it "should throw if symbol already exists" $
            run extract (do
                S.declareVar name value
                S.declareVar name value
                ) `shouldThrow` anyException

        it "should set variable to var if specified" $
            run extract (do
                S.declareVar name value
                S.isConst name
                ) >>= (`shouldBe` Just False)

        it "should set variable to const if specified" $
            run extract (do
                S.declareConst name value
                S.isConst name
                ) >>= (`shouldBe` Just True)

    describe "mustFind" $ do
        it "should return var if found" $
            run extract (do
                S.declareVar name value
                S.mustFind name
                ) >>= (`shouldBe` value)

        it "should throw if not found" $ do
            val <- run extract (
                S.mustFind name
                )
            (evaluate . force) val `shouldThrow` anyException -- unknown symbol

    describe "searchFor'" $ do
        it "should return local if found in local scope" $
            S.searchFor (Scope.find name) S.PState {
                S.stack = [scopeFromList [(name, value)], scopeFromList []],
                S.global = scopeFromList [(name, value')]
                } `shouldBe` Just value

        it "should return global value if not found in local scope" $
            S.searchFor (Scope.find name) S.PState {
                S.stack = [scopeFromList [], scopeFromList []],
                S.global = scopeFromList [(name, value')]
                } `shouldBe` Just value'

        it "should not look past the top of the stack" $
            S.searchFor (Scope.find name) S.PState {
                S.stack = [scopeFromList [], scopeFromList [(name, value'')]],
                S.global = scopeFromList []
                } `shouldBe` Nothing

    describe "findTop'" $ do
        it "should only search local scope if it exists" $
            S.findTop' name S.PState {
                S.stack = [scopeFromList []],
                S.global = scopeFromList [(name, value')]
                } `shouldBe` Nothing

        it "should return global value if stack is empty" $
            S.findTop' name S.PState {
                S.stack = [],
                S.global = scopeFromList [(name, value')]
                } `shouldBe` Just value'

    describe "applyTo" $ do
        it "should apply to local scope if it exists" $
            run extract (do
                put S.PState {
                    S.stack = [scopeFromList []],
                    S.global = scopeFromList []
                    }
                S.applyTo replaceWithCanary
                gets S.stack
                ) >>= (`shouldBe` [canaryScope])

        it "should apply to global scope if stack is empty" $
            run extract (do
                put S.PState {
                    S.stack = [],
                    S.global = scopeFromList []
                    }
                S.applyTo replaceWithCanary
                gets S.global
                ) >>= (`shouldBe` canaryScope)

    describe "overwrite" $
        it "should insert into top scope" $
            run extract (do
                put S.PState {
                    S.stack = [],
                    S.global = scopeFromList []
                    }
                S.overwrite name value'
                gets S.global
                ) >>= (`shouldBe` scopeFromList [(name, value')])

    describe "setConst" $
        it "should set name to const in top scope" $
            run extract (do
                put S.PState {
                    S.stack = [],
                    S.global = scopeFromList [(name, value')]
                    }
                S.setConst True name
                gets (Scope.isConst name . S.global)
                ) >>= (`shouldBe` Just True)

    describe "find'" $
        it "should find first match of name" $
            S.find' name S.PState {
                S.stack = [scopeFromList [(name, value)], scopeFromList []],
                S.global = scopeFromList [(name, value')]
                } `shouldBe` Just value

    describe "isConst" $
        it "should find first match of name" $
            run extract (do
                put S.PState {
                    S.stack = [scopeFromList [(name, value)], scopeFromList []],
                    S.global = Scope.setConst True name $ scopeFromList [(name, value')]
                    }
                S.isConst name
                ) >>= (`shouldBe` Just False)

    describe "push" $
        it "should push onto stack" $
            run extract (do
                S.push canaryScope
                gets S.stack
                ) >>= (`shouldBe` [canaryScope])

    describe "push" $
        it "should push Scope.empty onto stack" $
            run extract (do
                S.pushEmpty
                gets S.stack
                ) >>= (`shouldBe` [Scope.empty])

    describe "pop" $ do
        it "should pop from the stack if there's at least one scope" $
            run extract (do
                S.push canaryScope
                S.pushEmpty
                S.pop
                gets S.stack
                ) >>= (`shouldBe` [canaryScope])

        it "should throw error if stack is empty" $ do
            stack <- run extract (do
                S.pop
                gets S.stack
                )
            (evaluate . force) stack `shouldThrow` anyException

    describe "replace'" $ do
        it "should replace in local scope if found" $
            (S.stack . fromJust . S.replace' name value'') S.PState {
                S.stack = [scopeFromList [(name, value)]],
                S.global = scopeFromList [(name, value')]
                } `shouldBe` [scopeFromList [(name, value'')]]

        it "should replace in global scope if not found" $
            (S.global . fromJust . S.replace' name value'') S.PState {
                S.stack = [scopeFromList [], scopeFromList [(name, value)]],
                S.global = scopeFromList [(name, value')]
                } `shouldBe` scopeFromList [(name, value'')]

        it "should return nothing if not found in both local and global" $
            S.replace' name value'' S.PState {
                S.stack = [scopeFromList []],
                S.global = scopeFromList []
                } `shouldBe` Nothing

        it "should return nothing if local doesn't exist and not found in global" $
            S.replace' name value'' S.PState {
                S.stack = [],
                S.global = scopeFromList []
                } `shouldBe` Nothing

    describe "mustReplace" $ do
        it "should replace variable if found" $
            run extract (do
                put S.PState {
                    S.stack = [],
                    S.global = scopeFromList [(name, value')]
                    }
                S.mustReplace name value''
                gets (Scope.find name . S.global)
                ) >>= (`shouldBe` Just value'')

        it "should throw if not found found" $ do
            val <- run extract (S.mustReplace name value'')
            (evaluate . force) val `shouldThrow` anyException -- Unknown Symbol

    describe "getline" $ do
        it "should read exactly one line if there are multiple" $ do
            istream <- Streams.fromByteString $ pack "line1\nline2\n"
            runWithStreams istream Streams.stdout extract S.getline
                >>= (`shouldBe` "line1")

        it "should read until EOF if there are no newlines" $ do
            istream <- Streams.fromByteString $ pack "line1"
            runWithStreams istream Streams.stdout extract S.getline
                >>= (`shouldBe` "line1")

        it "should read empty string if stream is empty" $ do
            istream <- Streams.fromByteString $ pack ""
            runWithStreams istream Streams.stdout extract S.getline
                >>= (`shouldBe` "")

        it "should throw if stream is expended" $ do
            istream <- Streams.fromByteString $ pack ""
            line <- runWithStreams istream Streams.stdout extract (do
                _ <- S.getline
                S.getline
                )
            (evaluate . force) line `shouldThrow` anyException

    describe "putline" $
        it "should put exact string in the assigned output stream" $ do
            knob <- newKnob (pack [])
            h <- newFileHandle knob "~Pascal-Temp-InMemory.out" WriteMode
            outstream <- Streams.handleToOutputStream h

            runWithStreams Streams.stdin outstream extract (S.putline "line1\n")

            hClose h
            actual <- Data.Knob.getContents knob
            unpack actual `shouldBe` "line1\n"

