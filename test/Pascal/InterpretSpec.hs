module Pascal.InterpretSpec (spec) where

import           Control.Exception
import           Pascal.Data       as D
import           Pascal.Interpret  as I
import           Pascal.State      as S
import           Test.Hspec

extract :: S.AppReturn a -> a
extract st = case st of
    Right (a, _) -> a
    Left ev      -> throw ev

run :: (S.AppReturn a -> b) -> S.AppState a -> IO b
run extractor fn = do
    state <- S.runApp fn
    return $ extractor state

mockValues :: [S.Value]
mockValues = [
    S.BoolValue False,
    S.IntValue 2,
    S.FloatValue 2,
    S.StrValue "asdf",
    S.FuncValue mockFunc
    ]

mockNamedValues :: [S.Value]
mockNamedValues = map (\v -> S.NamedValue (D.Id . show . S.typeOf $ v) v) mockValues

makeCaseDecl :: [(Int, Int)] -> D.CaseDecl
makeCaseDecl cases = D.CaseDecl (map (uncurry D.IntRange) cases) (D.Stmts [])

isOfType :: D.PascalType -> S.Value -> Bool
isOfType t v = S.typeOf v == t

mockFunc :: D.Func
mockFunc = D.Func (Id "name1") [] TypeBool mockBlock

mockBlock :: D.Block
mockBlock = D.Block [] []

spec :: Spec
spec = do
    describe "declareNativeFunctions" $ do
        it "should declare all native functions" $ do
            let ids = map D.Id ["sqrt", "sin", "cos", "exp", "ln", "writeln", "readln"]

            Just vals <- run (sequence . extract) $ do
                I.declareNativeFunctions
                mapM S.find ids

            mapM_ (uncurry shouldBe) $ zip (map (S.nfName . S.getNativeFunc) vals) ids

    describe "evalExpr" $ do
        it "should cast literals to their corresponding values" $ do
            let exprs = [D.IntExpr 1, D.BoolExpr True, D.StrExpr "expr", D.FltExpr 1.0]
                vals = [S.IntValue 1, S.BoolValue True, S.StrValue "expr", S.FloatValue 1.0]

            Just actualVals <- run (sequence . extract) $ do
                mapM I.evalExpr exprs

            mapM_ (uncurry shouldBe) $ zip actualVals vals

        it "should return value if lookup VarExpr finds it" $ do
            let name = (D.Id "expr")
                expr' = D.VarExpr name
                val = S.NamedValue name $ S.BoolValue True

            (run extract $ do
                S.overwrite name val
                I.evalExpr expr') >>= (`shouldBe` (Just val))

        it "should return Nothing if lookup VarExpr doesnt find anything" $ do
            let name = (D.Id "expr")
                expr' = D.VarExpr name

            (run extract $ I.evalExpr expr') >>= (`shouldBe` Nothing)

        it "should combine LHS and RHS for binary expr if they're both OK" $ do
            let e1 = D.StrExpr "foo"
                e2 = D.StrExpr "bar"
                expr' = D.BinaryExpr "+" e1 e2
                val = S.StrValue "foobar"

            (run extract $ I.evalExpr expr') >>= (`shouldBe` (Just val))

        it "should return nothing for binary expr if either side is Nothing" $ do
            let e1 = D.StrExpr "e1"
                e2 = D.VarExpr $ D.Id "this shouldn't be found"
                expr12 = D.BinaryExpr "+" e1 e2
                expr21 = D.BinaryExpr "+" e2 e1

            (run extract $ I.evalExpr expr12) >>= (`shouldBe` Nothing)
            (run extract $ I.evalExpr expr21) >>= (`shouldBe` Nothing)

    describe "visitFuncCall" $ do
        it "should lookup and run native function" $ do
            let name = D.Id "myFunc"
                funcCall = D.FuncCall name []
                expectedValue = S.IntValue 1
                nativeFunc = S.NativeFuncValue $ S.NativeFunc name (\_ -> return $ Just expectedValue)

            (run extract $ do
                S.overwrite name nativeFunc
                I.visitFuncCall funcCall
                ) >>= (`shouldBe` (Just expectedValue))

        it "should throw exception if cannot find function implementation" $ do
            let name = D.Id "non-existent"
                funcCall = D.FuncCall name []

            (run extract $ do
                I.visitFuncCall funcCall
                ) `shouldThrow` anyException

        -- TODO more tests

    describe "xor" $ do
        it "should work in all possible cases" $ do
            I.xor True  True  `shouldBe` False
            I.xor True  False `shouldBe` True
            I.xor False True  `shouldBe` True
            I.xor False False `shouldBe` False

    describe "rvName" $ do
        it "should produce correct name for dummy function" $ do
            I.rvName mockFunc `shouldBe` (D.Id ".retval$name1")

    describe "cast" $ do
        it "should work as identity on same type" $ do
            mapM_ (\v -> I.cast (S.typeOf v) v `shouldBe` (Just v)) mockValues

        it "should lift named value before cast" $ do
            mapM_ (\(v, nv) -> I.cast (S.typeOf v) nv `shouldBe` (Just v)) $ zip mockValues mockNamedValues

        it "should cast int to float" $ do
            I.cast TypeFloat (S.IntValue 2) `shouldBe` (Just $ S.FloatValue 2)

        it "should refuse to cast float to int" $ do
            I.cast TypeInt (S.FloatValue 2) `shouldBe` Nothing

        it "should refuse to cast int to bool" $ do
            I.cast TypeBool (S.IntValue 2) `shouldBe` Nothing

        it "should refuse to cast bool to int" $ do
            I.cast TypeInt (S.BoolValue False) `shouldBe` Nothing

    describe "mustCast" $ do
        it "should return raw result if cast was ok" $ do
            let val = S.BoolValue False
            I.mustCast TypeBool val `shouldBe` val

        it "should throw something if cast failed" $ do
            let val = S.BoolValue False
            evaluate (I.mustCast TypeFunc val) `shouldThrow` anyException

    describe "splitAtSpace" $ do
        it "should split at the first space on a normal string" $ do
            I.splitAtSpace "my string is cool" `shouldBe` ("my", "string is cool")

        it "should split at the first character if that's a space" $ do
            I.splitAtSpace " space is first" `shouldBe` ("", "space is first")

        it "should split on multiple spaces but doesn't trim" $ do
            I.splitAtSpace "many  spaces" `shouldBe` ("many", " spaces")

        it "should split empty string" $ do
            I.splitAtSpace "" `shouldBe` ("", "")

        it "should split singleton string" $ do
            I.splitAtSpace "a" `shouldBe` ("a", "")

        it "should split singleton space" $ do
            I.splitAtSpace " " `shouldBe` ("", "")

        it "should work if there are no spaces" $ do
            I.splitAtSpace "abcdef" `shouldBe` ("abcdef", "")

        it "should split if the only space is the last character" $ do
            I.splitAtSpace "abcdef " `shouldBe` ("abcdef", "")

    describe "isvar" $ do
        it "should return true if a named value" $ do
            I.isvar (S.NamedValue (D.Id "myfunc") $ S.FuncValue mockFunc) `shouldBe` True

        it "should return false if not a named value" $ do
            I.isvar (S.FuncValue mockFunc) `shouldBe` False

    describe "doesMatch" $ do
        it "should reject anything except NamedValue and IntValue" $ do
            let decl = makeCaseDecl [(1, 1)]
            mapM_ (\val -> do
                evaluate (I.doesMatch val decl) `shouldThrow` anyException
                ) $ filter (not . (isOfType TypeInt)) mockValues

        it "should cast NamedValue to IntValue" $ do
            let decl = makeCaseDecl [(1, 1), (10, 10)]
            I.doesMatch (S.NamedValue (D.Id "myInt") $ S.IntValue 1) decl `shouldBe` True

        it "should doent cast NamedValue to anything besides IntValue" $ do
            let decl = makeCaseDecl [(1, 1)]
            mapM_ (\val -> do
                evaluate (I.doesMatch val decl) `shouldThrow` anyException
                ) $ filter (not . (isOfType TypeInt) . S.getValue) mockNamedValues

        it "return True if IntValue matches either end point" $ do
            let decl = makeCaseDecl [(1, 2)]
            I.doesMatch (S.IntValue 1) decl `shouldBe` True
            I.doesMatch (S.IntValue 2) decl `shouldBe` True

        it "return False if no match in list" $ do
            let decl = makeCaseDecl [(1, 1), (10, 10)]
            I.doesMatch (S.IntValue 11) decl `shouldBe` False

        it "return False if empty list" $ do
            let decl = makeCaseDecl []
            I.doesMatch (S.IntValue 1) decl `shouldBe` False

    describe "combineToNum" $ do
        it "should apply '+', '*', '-' to operands" $ do
            let ops = ["+", "*", "-"]
                results = [3, 2, -1]
                n1 = 1 :: Integer
                n2 = 2 :: Integer

            mapM_ (\(op, result) -> do
                (I.combineToNum op n1 n2, op) `shouldBe` (result, op)
                ) $ zip ops results

        it "should throw on unknown operand" $ do
            evaluate (I.combineToNum "/" (1 :: Integer) (2 :: Integer)) `shouldThrow` anyException

    describe "combineToBool" $ do
        it "should apply '<>', '=', '>', '<', '<=', '>=' to operands" $ do
            let ops = ["<>", "=", ">", "<", "<=", ">="]
                results = [True, False, False, True, True, False]
                n1 = 1 :: Integer
                n2 = 2 :: Integer

            mapM_ (\(op, result) -> do
                (I.combineToBool op n1 n2, op) `shouldBe` (result, op)
                ) $ zip ops results

        it "should throw on unknown operand" $ do
            evaluate (I.combineToBool "?" (1 :: Integer) (2 :: Integer)) `shouldThrow` anyException
