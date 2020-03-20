module Pascal.InterpretSpec (spec) where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.State  (get)
import           Pascal.Data          as D
import           Pascal.Interpret     as I
import           Pascal.State         as S
import           Test.Hspec

extract :: S.AppReturn a -> a
extract st = case st of
    (Right val, _) -> val
    (Left ev, _)   -> throw ev

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

    describe "evalFuncCall" $ do
        let makeNF name action = S.NativeFuncValue $ S.NativeFunc name action
            -- returns a S.NativeFuncValue that will return its `i`th argument
            makeProjection i = let
                name = (D.Id $ "get" ++ (show i))
                in makeNF name (\args -> return $ Just $ args!!i)

        it "should lookup and run native function" $ do
            let name = D.Id "myFunc"
                funcCall = D.FuncCall name []
                expectedValue = S.IntValue 1
                nativeFunc = makeNF name (\_ -> return $ Just expectedValue)

            (run extract $ do
                S.overwrite name nativeFunc
                I.evalFuncCall funcCall
                ) >>= (`shouldBe` (Just expectedValue))

        it "should throw exception if cannot find function implementation" $ do
            let name = D.Id "non-existent"
                funcCall = D.FuncCall name []

            (run extract $ do
                I.evalFuncCall funcCall
                ) `shouldThrow` anyException

        it "should evaluate arguments and pass them to native functions" $ do
            let name1 = D.Id "getFirst"
                e1 = D.StrExpr "foo"

            (run extract $ do
                S.overwrite name1 $ makeProjection 0
                I.evalFuncCall $ D.FuncCall name1 [e1]
                ) >>= (`shouldBe` (Just $ S.StrValue "foo"))

        it "should pass arguments to AST Funcs" $ do
            let funcName = D.Id "getFirst"
                paramName = D.Id "myParam"
                e1 = D.StrExpr "foo"
                funcBody = D.Block [] [AssignStmt funcName (D.VarExpr paramName)]
                f = D.Func {fname = funcName,
                            params = [D.Decl paramName D.TypeString],
                            returnType = D.TypeString,
                            block = funcBody}

            (run extract $ do
                S.overwrite funcName $ S.FuncValue f
                I.evalFuncCall $ D.FuncCall funcName [e1]
                ) >>= (`shouldBe` (Just $ S.StrValue "foo"))

    describe "visitWhileStmt" $ do
        let dummyVarName = D.Id "dummyVar"
            dummyVarRef = D.VarExpr dummyVarName
            unknownVarName = D.Id "unknown"
            unknownVarRef = D.VarExpr unknownVarName
            zero = D.IntExpr 0
            one = D.IntExpr 1
            decrementDummyVar = D.AssignStmt dummyVarName $ D.BinaryExpr "-" dummyVarRef one

            makeNamedInt :: Int -> S.Value
            makeNamedInt n = S.NamedValue dummyVarName $ S.IntValue n

            makeComparisonTo :: String -> Int -> D.Expr -> D.Expr
            makeComparisonTo op n lhs = D.BinaryExpr op lhs $ D.IntExpr n

            {-
            Equivalent to:
            dummyVar := n;
            while dummyVar > 0 begin
                dummyVar := dummyVar - 1;
                // execute `stmts`
            end;
            -}
            setupWhileNTimes :: Int -> [D.Stmt] -> S.AppState (D.Stmt)
            setupWhileNTimes n stmts = do
                let dummyVar = makeNamedInt n
                S.declareVar dummyVarName dummyVar
                return D.WhileStmt {getWhileExpr = D.BinaryExpr ">" dummyVarRef zero,
                                    getWhileStmt = D.Stmts $ [decrementDummyVar] ++ stmts}

        it "should throw if it can't evaluate the while condition" $ do
            let whileStmt = D.WhileStmt {getWhileExpr = unknownVarRef,
                                         getWhileStmt = D.Stmts []}
            (run extract $ do
                I.visitWhileStmt whileStmt
                ) `shouldThrow` anyException -- cannot eval

        it "should throw if the while condition is of incorrect type" $ do
            let whileStmt = D.WhileStmt {getWhileExpr = D.IntExpr 1,
                                         getWhileStmt = D.Stmts []}
            (run extract $ do
                I.visitWhileStmt whileStmt
                ) `shouldThrow` anyException -- incorrect type

        it "should correctly reference the state in the while condition" $ do
            (run extract $ do
                whileStmt <- setupWhileNTimes 3 []
                I.visitWhileStmt whileStmt
                S.find dummyVarName
                ) >>= (`shouldBe` (Just $ makeNamedInt 0))

        it "should preserve state on break" $ do
            (run extract $ do
                whileStmt <- setupWhileNTimes 3 [D.IfStmt (makeComparisonTo "=" 1 dummyVarRef) D.BreakStmt]
                catchError (I.visitWhileStmt whileStmt) (\evt -> case evt of
                    S.Break -> return ()
                    _       -> error "expected break"
                    )
                S.find dummyVarName
                ) >>= (`shouldBe` (Just $ makeNamedInt 1))

        it "should skip remaining statements on continue" $ do
            let varName = D.Id "lastSeen"
            (run extract $ do
                S.declareVar varName (S.IntValue $ -1)
                whileStmt <- setupWhileNTimes 3 [
                    D.IfStmt (makeComparisonTo "=" 0 dummyVarRef) D.ContinueStmt,
                    D.AssignStmt varName dummyVarRef
                    ]
                
                I.visitWhileStmt whileStmt
                (Just var) <- S.find varName
                return $ I.cast D.TypeInt var
                ) >>= (`shouldBe` (Just $ S.IntValue 1))

    describe "visitForStmt" $ do
        let nameI = D.Id "i"
            refI = D.VarExpr nameI
            zero = S.IntValue 0
            zeroExpr = D.IntExpr 0
            oneExpr = D.IntExpr 1
            badExpr = D.VarExpr $ D.Id "doesntExist"
            forTo = visitForStmt True
            nameLoopCount = D.Id "loopCount"
            incrementLoopCount = D.AssignStmt nameLoopCount $ D.BinaryExpr "+" (D.VarExpr nameLoopCount) oneExpr

            {-
            Equivalent to 
            var loopCount : integer = 0;
                i : integer;
            // ...
            for i := `begin` (to/downTo) `end` do begin
                // execute `stmts`
                myIters := myIters + 1;
            -}
            runCountLoops :: Bool -> Int -> Int -> [D.Stmt] -> S.AppState ()
            runCountLoops isUp begin end stmts = do
                S.declareVar nameLoopCount $ S.NamedValue nameLoopCount zero
                S.declareVar nameI zero
                visitForStmt isUp nameI (D.IntExpr begin) (D.IntExpr end) (Stmts $ stmts ++ [incrementLoopCount])

            runCountLoopsUp :: Int -> Int -> [D.Stmt] -> S.AppState ()
            runCountLoopsUp = runCountLoops True

        it "should throw if `start` cannot be evaluated" $ do
            (run extract $ do
                S.declareVar nameI zero
                forTo nameI badExpr zeroExpr (Stmts [])
                ) `shouldThrow` anyException -- cannot eval
        
        it "should throw if `end` cannot be evaluated" $ do
            (run extract $ do
                S.declareVar nameI zero
                forTo nameI zeroExpr badExpr (Stmts [])
                ) `shouldThrow` anyException -- cannot eval
        
        it "should throw if iteration var is const" $ do
            (run extract $ do
                S.declareConst nameI zero
                forTo nameI zeroExpr oneExpr (Stmts [])
                ) `shouldThrow` anyException -- cannot assign to const

        it "should throw if iteration var is undeclared" $ do
            (run extract $ do
                forTo nameI zeroExpr oneExpr (Stmts [])
                ) `shouldThrow` anyException -- cannot assign to const
    
        it "should throw if iteration var is not of type IntValue" $ do
            (run extract $ do
                S.declareVar nameI $ S.FloatValue 1.0
                forTo nameI zeroExpr oneExpr (Stmts [])
                ) `shouldThrow` anyException -- type mismatch

        it "should throw if iteration var is not of type IntValue" $ do
            (run extract $ do
                S.declareVar nameI $ S.FloatValue 1.0
                forTo nameI zeroExpr oneExpr (Stmts [])
                ) `shouldThrow` anyException -- incorrect type

        it "should allow modifying variables in outer scopes" $ do
            (run extract $ do
                runCountLoopsUp 1 5 []
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 5))

        it "should not execute loop if lo > hi" $ do
            (run extract $ do
                runCountLoopsUp 1 0 []
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 0))

        it "should not execute `downTo` loop if begin < end" $ do
            (run extract $ do
                runCountLoops False 0 1 []
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 0))

        it "should execute `downTo` loop if begin > end" $ do
            (run extract $ do
                runCountLoops False 1 0 []
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 2))

        it "should not run subsequent statements on continue" $ do
            (run extract $ do
                runCountLoopsUp 1 5 [
                    D.IfStmt (D.BinaryExpr "=" refI oneExpr) D.ContinueStmt]
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 4))

        it "should not set iteration variable on break" $ do
            (run extract $ do
                runCountLoopsUp 1 5 [
                    D.IfStmt (D.BinaryExpr "=" refI oneExpr) D.BreakStmt]
                S.find nameI
                ) >>= (`shouldBe` (Just $ S.NamedValue nameI $ S.IntValue 1))

        it "should not run subsequent statements on break" $ do
            (run extract $ do
                runCountLoopsUp 1 5 [
                    D.IfStmt (D.BinaryExpr "=" refI oneExpr) D.BreakStmt]
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 0))

        it "should leave iteration variable non-const after the loop" $ do
            (run extract $ do
                runCountLoopsUp 1 5 []
                S.isConst nameI
                ) >>= (`shouldBe` (Just False))

    describe "mustEvalExpr" $ do
        let badExpr = D.VarExpr $ D.Id "doesntExist"
            goodExpr = D.IntExpr 1
            val = S.IntValue 1

        it "should evaluate and return expression if its a Just" $ do
            (run extract $ do
                I.mustEvalExpr goodExpr
                ) >>= (`shouldBe` val)

        it "should throw error if it gets a Nothing" $ do
            (run extract $ do
                I.mustEvalExpr badExpr
                ) `shouldThrow` anyException

    describe "evalFuncValue" $ do
        let funcName = D.Id "getFirst"
            paramName1 = D.Id "strParam"
            paramName2 = D.Id "intParam"
            parentScopeVarName = D.Id "inParentScope"
            namedValue1 = S.NamedValue parentScopeVarName $ S.StrValue "original value"
            namedValue2 = S.NamedValue parentScopeVarName $ S.StrValue "new value"
            v1 = S.StrValue "foo"
            v2 = S.IntValue 1
            returnFirstArg = D.Func {
                fname = funcName,
                params = [D.Decl paramName1 D.TypeString, D.Decl paramName2 D.TypeInt],
                returnType = D.TypeString,
                block = D.Block {
                    blockDecls = [],
                    blockStmts = [D.AssignStmt funcName (D.VarExpr paramName1)]
                    }
                }
            accessFirstArgReturnNone = D.Func {
                fname = funcName,
                params = [D.Decl paramName1 D.TypeString],
                returnType = D.TypeNone,
                block = D.Block {
                    blockDecls = [],
                    -- access arg here
                    blockStmts = [D.AssignStmt paramName1 (D.VarExpr paramName1)]
                    }
                }
            accessReturnButReturnNone = D.Func {
                fname = funcName,
                params = [],
                returnType = D.TypeNone,
                block = D.Block {
                    blockDecls = [],
                    -- should fail, since return value doesn't exist
                    blockStmts = [D.AssignStmt funcName (D.IntExpr 1)]
                    }
                }
            returnDefault = D.Func {
                fname = funcName,
                params = [],
                returnType = D.TypeInt,
                block = D.Block {
                    blockDecls = [],
                    blockStmts = []
                    }
                }
            accessParentScope = D.Func {
                fname = funcName,
                params = [D.Decl paramName1 D.TypeString],
                returnType = D.TypeNone,
                block = D.Block {
                    blockDecls = [],
                    blockStmts = [D.AssignStmt parentScopeVarName (D.VarExpr paramName1)]
                    }
                }
            duplicateDeclaration = D.Func {
                fname = funcName,
                params = [
                    D.Decl paramName1 D.TypeString,
                    D.Decl paramName1 D.TypeString
                    ],
                returnType = D.TypeNone,
                block = D.Block {
                    blockDecls = [],
                    blockStmts = []
                    }
                }
        it "should make arguments and return values accessible" $ do
            (run extract $ do
                -- function must be on the stack, to trigger conversion to return value
                S.declareVar funcName $ S.FuncValue returnFirstArg

                I.evalFuncValue returnFirstArg [v1, v2]
                ) >>= (`shouldBe` (Just v1))

        it "should return Nothing if no return value" $ do
            (run extract $ do
                I.evalFuncValue accessFirstArgReturnNone [v1]
                ) >>= (`shouldBe` Nothing)

        it "shouldn't put return value on stack when return type is TypeNone" $ do
            (run extract $ do
                I.evalFuncValue accessReturnButReturnNone []
                ) `shouldThrow` anyException -- illegal access

        it "should set return value to default value for type" $ do
            (run extract $ do
                I.evalFuncValue returnDefault []
                ) >>= (`shouldBe` (Just $ S.IntValue 0))

        it "should allow changes to variables from global scope" $ do
            (run extract $ do
                S.overwrite parentScopeVarName namedValue1
                _ <- I.evalFuncValue accessParentScope [namedValue2]
                S.find parentScopeVarName
                ) >>= (`shouldBe` (Just namedValue2))

        it "should not allow duplicate parameter names" $ do
            (run extract $ do
                I.evalFuncValue duplicateDeclaration [namedValue1, namedValue2]
                ) `shouldThrow` anyException -- duplicate declaration

        it "should leave stack unchanged after execution" $ do
            (stack1, stack2) <- (run extract $ do
                S.declareVar funcName $ S.FuncValue returnFirstArg
                stack1 <- get
                _ <- I.evalFuncValue returnFirstArg [v1, v2]
                stack2 <- get
                return (stack1, stack2)
                )
            stack2 `shouldBe` stack1

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
