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
        it "declares all native functions" $ do
            let ids = map D.Id ["sqrt", "sin", "cos", "exp", "ln", "writeln", "readln"]

            Just vals <- run (sequence . extract) $ do
                I.declareNativeFunctions
                mapM S.find ids

            mapM_ (uncurry shouldBe) $ zip (map (S.nfName . S.getNativeFunc) vals) ids

    describe "xor" $ do
        it "works in all possible cases" $ do
            I.xor True True `shouldBe` False
            I.xor True False `shouldBe` True
            I.xor False True `shouldBe` True
            I.xor False False `shouldBe` False

    describe "rvName" $ do
        it "produces correct name for dummy function" $ do
            I.rvName mockFunc `shouldBe` (D.Id ".retval$name1")

    describe "cast" $ do
        it "works as identity on same type" $ do
            mapM_ (\v -> I.cast (S.typeOf v) v `shouldBe` (Just v)) mockValues

        it "lifts named value before cast" $ do
            mapM_ (\(v, nv) -> I.cast (S.typeOf v) nv `shouldBe` (Just v)) $ zip mockValues mockNamedValues

        it "casts int to float" $ do
            I.cast TypeFloat (S.IntValue 2) `shouldBe` (Just $ S.FloatValue 2)

        it "refuses to cast float to int" $ do
            I.cast TypeInt (S.FloatValue 2) `shouldBe` Nothing

        it "refuses to cast int to bool" $ do
            I.cast TypeBool (S.IntValue 2) `shouldBe` Nothing

        it "refuses to cast bool to int" $ do
            I.cast TypeInt (S.BoolValue False) `shouldBe` Nothing

    describe "mustCast" $ do
        it "returns raw result if cast was ok" $ do
            let val = S.BoolValue False
            I.mustCast TypeBool val `shouldBe` val

        it "throws something if cast failed" $ do
            let val = S.BoolValue False
            evaluate (I.mustCast TypeFunc val) `shouldThrow` anyException

    describe "splitAtSpace" $ do
        it "splits at the first space on a normal string" $ do
            I.splitAtSpace "my string is cool" `shouldBe` ("my", "string is cool")

        it "splits at the first character if that's a space" $ do
            I.splitAtSpace " space is first" `shouldBe` ("", "space is first")

        it "splits on multiple spaces but doesn't trim" $ do
            I.splitAtSpace "many  spaces" `shouldBe` ("many", " spaces")

        it "splits empty string" $ do
            I.splitAtSpace "" `shouldBe` ("", "")

        it "splits singleton string" $ do
            I.splitAtSpace "a" `shouldBe` ("a", "")

        it "splits singleton space" $ do
            I.splitAtSpace " " `shouldBe` ("", "")

        it "works if there are no spaces" $ do
            I.splitAtSpace "abcdef" `shouldBe` ("abcdef", "")

        it "splits if the only space is the last character" $ do
            I.splitAtSpace "abcdef " `shouldBe` ("abcdef", "")

    describe "isvar" $ do
        it "returns true if a named value" $ do
            I.isvar (S.NamedValue (D.Id "myfunc") $ S.FuncValue mockFunc) `shouldBe` True

        it "returns false if not a named value" $ do
            I.isvar (S.FuncValue mockFunc) `shouldBe` False

    describe "doesMatch" $ do
        it "rejects anything except NamedValue and IntValue" $ do
            let decl = makeCaseDecl [(1, 1)]
            mapM_ (\val -> do
                evaluate (I.doesMatch val decl) `shouldThrow` anyException
                ) $ filter (not . (isOfType TypeInt)) mockValues

        it "casts NamedValue to IntValue" $ do
            let decl = makeCaseDecl [(1, 1), (10, 10)]
            I.doesMatch (S.NamedValue (D.Id "myInt") $ S.IntValue 1) decl `shouldBe` True

        it "doesnt cast NamedValue to anything besides IntValue" $ do
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
        it "applies '+', '*', '-' to operands" $ do
            let ops = ["+", "*", "-"]
                results = [3, 2, -1]
                n1 = 1 :: Integer
                n2 = 2 :: Integer

            mapM_ (\(op, result) -> do
                (I.combineToNum op n1 n2, op) `shouldBe` (result, op)
                ) $ zip ops results

        it "throws on unknown operand" $ do
            evaluate (I.combineToNum "/" (1 :: Integer) (2 :: Integer)) `shouldThrow` anyException

    describe "combineToBool" $ do
        it "applies '<>', '=', '>', '<', '<=', '>=' to operands" $ do
            let ops = ["<>", "=", ">", "<", "<=", ">="]
                results = [True, False, False, True, True, False]
                n1 = 1 :: Integer
                n2 = 2 :: Integer

            mapM_ (\(op, result) -> do
                (I.combineToBool op n1 n2, op) `shouldBe` (result, op)
                ) $ zip ops results

        it "throws on unknown operand" $ do
            evaluate (I.combineToBool "?" (1 :: Integer) (2 :: Integer)) `shouldThrow` anyException
