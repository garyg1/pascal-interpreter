module Pascal.InterpretSpec (spec) where

import           Control.DeepSeq
import           Control.Exception    (evaluate)
import           Control.Monad.Except
import           Control.Monad.State  (get)
import           Data.Maybe           (fromJust)
import qualified Pascal.Data          as D
import qualified Pascal.Interpret     as I
import qualified Pascal.State         as S
import           Pascal.TestUtils     (extract, run)
import           Test.Hspec

-- TODO test combine
-- TODO test marshal

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
mockFunc = D.Func (D.Id "name1") [] D.TypeBool mockBlock

mockBlock :: D.Block
mockBlock = D.Block [] []

makeNF :: D.Id -> ([S.Value] -> S.AppState (Maybe S.Value)) -> S.Value
makeNF name action = S.NativeFuncValue $ S.NativeFunc name action

-- returns a S.NativeFuncValue that will return its `i`th argument
makeProjection :: Int -> S.Value
makeProjection i = let
    name = (D.Id $ "get" ++ show i)
    in makeNF name (\args -> return $ Just $ args!!i)

spec :: Spec
spec = do
    describe "declareNativeFunctions" $
        it "should declare all native functions" $ do
            let ids = map D.Id ["sqrt", "sin", "cos", "exp", "ln", "writeln", "readln"]

            Just vals <- run (sequence . extract) $ do
                I.declareNativeFunctions
                mapM S.find ids

            mapM_ (uncurry shouldBe) $ zip ids $ map (S.nfName . S.getNativeFunc) vals

    describe "visitVarDecl" $ do
        it "should put default value on stack for Decl" $
            run extract (do
                I.visitVarDecl False $ D.Decl (D.Id "foo") D.TypeBool
                S.mustFind (D.Id "foo")
                ) >>= (`shouldBe` S.NamedValue (D.Id "foo") (S.BoolValue False))

        it "should put expr result on stack for DeclDefn" $
            run extract (do
                I.visitVarDecl False $ D.DeclDefn (D.Id "foo") (D.BoolExpr True)
                S.mustFind (D.Id "foo")
                ) >>= (`shouldBe` S.NamedValue (D.Id "foo") (S.BoolValue True))

        it "should throw if cannot cast expr to type of declaration" $ do
            val <- run extract (do
                I.visitVarDecl False $ D.DeclTypeDefn (D.Id "foo") D.TypeInt (D.BoolExpr True)
                S.mustFind (D.Id "foo")
                )
            (evaluate . force) val `shouldThrow` anyException -- cannot cast

        it "should cast to declared type and assign if allowed" $
            run extract (do
                I.visitVarDecl False $ D.DeclTypeDefn (D.Id "foo") D.TypeFloat (D.IntExpr 1)
                S.mustFind (D.Id "foo")
                ) >>= (pure . S.getValue)
                  >>= (`shouldBe` S.FloatValue 1.0)

        it "should set const-ness of variable in state" $
            run extract (do
                I.visitVarDecl True $ D.Decl (D.Id "foo") D.TypeBool
                S.isConst (D.Id "foo")
                ) >>= (`shouldBe` Just True)

    describe "visitStmt" $ do
        it "should throw an Event on BreakStmt" $
            run extract (catchError (I.visitStmt D.BreakStmt) \case
                S.Break -> return ()
                _       -> error "expected break"
                ) >>= (`shouldBe` ())

        it "should throw an Event on ContinueStmt" $
            run extract (catchError (I.visitStmt D.ContinueStmt) \case
                S.Continue -> return ()
                _          -> error "expected continue"
                ) >>= (`shouldBe` ())

        it "should catch Break events on WhileStmt" $
            run extract (
                I.visitStmt $ D.WhileStmt (D.BoolExpr True) D.BreakStmt
                ) >>= (`shouldBe` ())


    describe "evalExpr" $ do
        it "should cast literals to their corresponding values" $ do
            let exprs = [D.IntExpr 1, D.BoolExpr True, D.StrExpr "expr", D.FltExpr 1.0]
                vals = [S.IntValue 1, S.BoolValue True, S.StrValue "expr", S.FloatValue 1.0]

            Just actualVals <- run (sequence . extract) $ mapM I.evalExpr exprs

            mapM_ (uncurry shouldBe) $ zip actualVals vals

        it "should return value if lookup VarExpr finds it" $ do
            let name = D.Id "expr"
                expr' = D.VarExpr name
                val = S.NamedValue name $ S.BoolValue True

            run extract (do
                S.overwrite name val
                I.evalExpr expr') >>= (`shouldBe` Just val)

        it "should return Nothing if lookup VarExpr doesnt find anything" $ do
            let name = D.Id "expr"
                expr' = D.VarExpr name

            run extract (I.evalExpr expr') >>= (`shouldBe` Nothing)

        it "should combine LHS and RHS for binary expr if they're both OK" $ do
            let e1 = D.StrExpr "foo"
                e2 = D.StrExpr "bar"
                expr' = D.BinaryExpr "+" e1 e2
                val = S.StrValue "foobar"

            run extract (I.evalExpr expr') >>= (`shouldBe` Just val)

        it "should return nothing for binary expr if either side is Nothing" $ do
            let e1 = D.StrExpr "e1"
                e2 = D.VarExpr $ D.Id "this shouldn't be found"
                expr12 = D.BinaryExpr "+" e1 e2
                expr21 = D.BinaryExpr "+" e2 e1

            run extract (I.evalExpr expr12) >>= (`shouldBe` Nothing)
            run extract (I.evalExpr expr21) >>= (`shouldBe` Nothing)

    describe "evalFuncCall" $ do
        it "should lookup and run native function" $ do
            let name = D.Id "myFunc"
                funcCall = D.FuncCall name []
                expectedValue = S.IntValue 1
                nativeFunc = makeNF name (\_ -> return $ Just expectedValue)

            run extract (do
                S.overwrite name nativeFunc
                I.evalFuncCall funcCall
                ) >>= (`shouldBe` Just expectedValue)

        it "should throw exception if cannot find function implementation" $ do
            let name = D.Id "non-existent"
                funcCall = D.FuncCall name []

            run extract (
                I.evalFuncCall funcCall
                ) `shouldThrow` anyException

        it "should evaluate arguments and pass them to native functions" $ do
            let name1 = D.Id "getFirst"
                e1 = D.StrExpr "foo"

            run extract (do
                S.overwrite name1 $ makeProjection 0
                I.evalFuncCall $ D.FuncCall name1 [e1]
                ) >>= (pure . fromJust)
                  >>= (`shouldBe` S.StrValue "foo")

        it "should pass arguments to AST Funcs" $ do
            let funcName = D.Id "getFirst"
                paramName = D.Id "myParam"
                e1 = D.StrExpr "foo"
                funcBody = D.Block [] [D.AssignStmt funcName (D.VarExpr paramName)]
                f = D.Func {D.fname = funcName,
                            D.params = [D.Decl paramName D.TypeString],
                            D.returnType = D.TypeString,
                            D.block = funcBody}
            run extract (do
                S.overwrite funcName $ S.FuncValue f
                I.evalFuncCall $ D.FuncCall funcName [e1]
                ) >>= (pure . S.getValue . fromJust)
                  >>= (`shouldBe` S.StrValue "foo")

    describe "visitIfElseStmt" $ do
        let unknownVarName = D.Id "unknown"
            unknownVarExpr = D.VarExpr unknownVarName
            intExpr = D.IntExpr 1
            trueExpr = D.BoolExpr True
            falseExpr = D.BoolExpr False
            passStmt = D.Stmts []
            throwStmt = error "error, unexpected"

        it "should throw if cannot evalutate if expression" $
            run extract (
                I.visitIfElseStmt unknownVarExpr passStmt passStmt
                ) `shouldThrow` anyException

        it "should throw if cannot cast if expression result to Bool" $
            run extract (
                I.visitIfElseStmt intExpr passStmt passStmt
                ) `shouldThrow` anyException

        it "should run the first statement if the 'if' expression is True" $
            run extract (
                I.visitIfElseStmt trueExpr passStmt throwStmt
                ) >>= (`shouldBe` ())

        it "should run the second statement if the 'if' expression is False" $
            run extract (
                I.visitIfElseStmt falseExpr throwStmt passStmt
                ) >>= (`shouldBe` ())

    describe "visitAssignStmt" $ do
        let unknownVarName = D.Id "unknown"
            name1 = D.Id "exists1"
            name2 = D.Id "exists2"
            twoExpr = D.IntExpr 2
            badExpr = D.VarExpr unknownVarName
            mockFunc1 = D.Func (D.Id "name1") [] D.TypeBool mockBlock
            mockFunc2 = D.Func (D.Id "name2") [] D.TypeBool mockBlock

        it "should throw if cannot evaluate RHS expression" $
            run extract (do
                S.declareVar name1 $ S.IntValue 1
                I.visitAssignStmt name1 badExpr
                ) `shouldThrow` anyException

        it "should throw if LHS variable is undeclared" $
            run extract (
                I.visitAssignStmt unknownVarName twoExpr
                ) `shouldThrow` anyException

        it "should evaluate RHS expr and assign it to the LHS name if they are of same non-FuncValue type" $
            run extract (do
                S.declareVar name1 $ S.IntValue 1
                I.visitAssignStmt name1 twoExpr
                S.find name1
                ) >>= (`shouldBe` Just (S.NamedValue name1 $ S.IntValue 2))

        it "should evaluate RHS expr and assign it to the LHS name if they are both of type FuncValue" $
            run extract (do
                S.declareVar name1 $ S.FuncValue mockFunc1
                S.declareVar name2 $ S.FuncValue mockFunc2
                I.visitAssignStmt name1 $ D.VarExpr name2
                S.find name1
                ) >>= (`shouldBe` Just (S.FuncValue mockFunc2))

        it "should cast RHS to LHS returnType if LHS is a FuncValue and RHS is not" $ do
            let retValName = I.rvName mockFunc1
            run extract (do
                S.declareVar name1 $ S.FuncValue mockFunc1
                S.declareVar retValName $ S.BoolValue False
                S.declareVar name2 $ S.BoolValue True

                I.visitAssignStmt name1 $ D.VarExpr name2

                S.find retValName
                ) >>= (pure . S.getValue . fromJust)
                  >>= (`shouldBe` S.BoolValue True)

        it "should allow NativeFunctions to be assigned to each other" $
            run extract (do
                S.declareVar name1 $ makeProjection 0
                S.declareVar name2 $ makeProjection 1
                I.visitAssignStmt name1 $ D.VarExpr name2
                S.mustFind name1
                ) >>= (`shouldBe` makeProjection 1)

    describe "visitCaseElseStmt" $ do
        let unknownVarName = D.Id "unknown"
            thenThrow = error "expected"
            thenPass = D.Stmts []
            oneExpr = D.IntExpr 1
            match1 = D.IntRange 1 1
            match2 = D.IntRange 2 2

        it "should throw if cannot evaluate case expression" $ do
            let caseDecls = [D.CaseDecl [match1] thenPass]
            run extract (
                I.visitCaseElseStmt (D.VarExpr unknownVarName) caseDecls (D.Stmts [])
                ) `shouldThrow` anyException

        it "should throw if there are no case declarations" $
            run extract (
                I.visitCaseElseStmt oneExpr [] (D.Stmts [])
                ) `shouldThrow` anyException

        it "should throw if case expression is not an IntValue" $ do
            let caseDecls = [D.CaseDecl [match1] thenPass]
            run extract (
                I.visitCaseElseStmt (D.FltExpr 1.0) caseDecls (D.Stmts [])
                ) `shouldThrow` anyException

        it "should visit case statement of first matching case declaration" $ do
            let caseDecls = [D.CaseDecl [match1] thenPass,
                             D.CaseDecl [match1] thenThrow]
            run extract (
                I.visitCaseElseStmt oneExpr caseDecls thenThrow
                ) >>= (`shouldBe` ())

        it "should not visit case statement of a subsequent matching case declaration" $ do
            let caseDecls = [D.CaseDecl [match1] thenPass,
                             D.CaseDecl [match1] thenThrow]
            run extract (
                I.visitCaseElseStmt oneExpr caseDecls thenThrow
                ) >>= (`shouldBe` ())

        it "should visit else statement if no cases match" $ do
            let caseDecls = [D.CaseDecl [match2] thenThrow,
                             D.CaseDecl [match2] thenThrow]
            run extract (
                I.visitCaseElseStmt oneExpr caseDecls thenPass
                ) >>= (`shouldBe` ())

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
            setupWhileNTimes :: Int -> [D.Stmt] -> S.AppState D.Stmt
            setupWhileNTimes n stmts = do
                let dummyVar = makeNamedInt n
                S.declareVar dummyVarName dummyVar
                return D.WhileStmt {D.getWhileExpr = D.BinaryExpr ">" dummyVarRef zero,
                                    D.getWhileStmt = D.Stmts $ decrementDummyVar : stmts}

        it "should throw if it can't evaluate the while condition" $ do
            let whileStmt = D.WhileStmt {D.getWhileExpr = unknownVarRef,
                                         D.getWhileStmt = D.Stmts []}
            run extract (
                I.visitWhileStmt whileStmt
                ) `shouldThrow` anyException -- cannot eval

        it "should throw if the while condition is of incorrect type" $ do
            let whileStmt = D.WhileStmt {D.getWhileExpr = D.IntExpr 1,
                                         D.getWhileStmt = D.Stmts []}
            run extract (
                I.visitWhileStmt whileStmt
                ) `shouldThrow` anyException -- incorrect type

        it "should correctly reference the state in the while condition" $
            run extract (do
                whileStmt <- setupWhileNTimes 3 []
                I.visitWhileStmt whileStmt
                S.find dummyVarName
                ) >>= (`shouldBe` (Just $ makeNamedInt 0))

        it "should preserve state on break" $
            run extract (do
                whileStmt <- setupWhileNTimes 3 [D.IfStmt (makeComparisonTo "=" 1 dummyVarRef) D.BreakStmt]
                catchError (I.visitWhileStmt whileStmt) (\case
                    S.Break -> return ()
                    _       -> error "expected break"
                    )
                S.find dummyVarName
                ) >>= (`shouldBe` (Just $ makeNamedInt 1))

        it "should skip remaining statements on continue" $ do
            let varName = D.Id "lastSeen"
            run extract (do
                S.declareVar varName (S.IntValue $ -1)
                whileStmt <- setupWhileNTimes 3 [
                    D.IfStmt (makeComparisonTo "=" 0 dummyVarRef) D.ContinueStmt,
                    D.AssignStmt varName dummyVarRef
                    ]

                I.visitWhileStmt whileStmt
                S.find varName
                ) >>= (pure . I.mustCast D.TypeInt . fromJust)
                  >>= (`shouldBe` S.IntValue 1)

    describe "visitForStmt" $ do
        let nameI = D.Id "i"
            refI = D.VarExpr nameI
            zero = S.IntValue 0
            zeroExpr = D.IntExpr 0
            oneExpr = D.IntExpr 1
            badExpr = D.VarExpr $ D.Id "doesntExist"
            forTo = I.visitForStmt True
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
                I.visitForStmt isUp nameI (D.IntExpr begin) (D.IntExpr end)
                    (D.Stmts $ stmts ++ [incrementLoopCount])

            runCountLoopsUp :: Int -> Int -> [D.Stmt] -> S.AppState ()
            runCountLoopsUp = runCountLoops True

        it "should throw if `start` cannot be evaluated" $
            run extract (do
                S.declareVar nameI zero
                forTo nameI badExpr zeroExpr (D.Stmts [])
                ) `shouldThrow` anyException -- cannot eval

        it "should throw if `end` cannot be evaluated" $
            run extract (do
                S.declareVar nameI zero
                forTo nameI zeroExpr badExpr (D.Stmts [])
                ) `shouldThrow` anyException -- cannot eval

        it "should throw if iteration var is const" $
            run extract (do
                S.declareConst nameI zero
                forTo nameI zeroExpr oneExpr (D.Stmts [])
                ) `shouldThrow` anyException -- cannot assign to const

        it "should throw if iteration var is undeclared" $
            run extract (
                forTo nameI zeroExpr oneExpr (D.Stmts [])
                ) `shouldThrow` anyException -- cannot assign to const

        it "should throw if iteration var is not of type IntValue" $
            run extract (do
                S.declareVar nameI $ S.FloatValue 1.0
                forTo nameI zeroExpr oneExpr (D.Stmts [])
                ) `shouldThrow` anyException -- type mismatch

        it "should throw if iteration var is not of type IntValue" $
            run extract (do
                S.declareVar nameI $ S.FloatValue 1.0
                forTo nameI zeroExpr oneExpr (D.Stmts [])
                ) `shouldThrow` anyException -- incorrect type

        it "should allow modifying variables in outer scopes" $
            run extract (do
                runCountLoopsUp 1 5 []
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 5))

        it "should not execute loop if lo > hi" $
            run extract (do
                runCountLoopsUp 1 0 []
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 0))

        it "should not execute `downTo` loop if begin < end" $
            run extract (do
                runCountLoops False 0 1 []
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 0))

        it "should execute `downTo` loop if begin > end" $
            run extract (do
                runCountLoops False 1 0 []
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 2))

        it "should not run subsequent statements on continue" $
            run extract (do
                runCountLoopsUp 1 5 [
                    D.IfStmt (D.BinaryExpr "=" refI oneExpr) D.ContinueStmt]
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 4))

        it "should not set iteration variable on break" $
            run extract (do
                runCountLoopsUp 1 5 [
                    D.IfStmt (D.BinaryExpr "=" refI oneExpr) D.BreakStmt]
                S.find nameI
                ) >>= (`shouldBe` (Just $ S.NamedValue nameI $ S.IntValue 1))

        it "should not run subsequent statements on break" $
            run extract (do
                runCountLoopsUp 1 5 [
                    D.IfStmt (D.BinaryExpr "=" refI oneExpr) D.BreakStmt]
                S.find nameLoopCount
                ) >>= (`shouldBe` (Just $ S.NamedValue nameLoopCount $ S.IntValue 0))

        it "should leave iteration variable non-const after the loop" $
            run extract (do
                runCountLoopsUp 1 5 []
                S.isConst nameI
                ) >>= (`shouldBe` Just False)

    describe "mustEvalExpr" $ do
        let badExpr = D.VarExpr $ D.Id "doesntExist"
            goodExpr = D.IntExpr 1
            val = S.IntValue 1

        it "should evaluate and return expression if its a Just" $
            run extract (
                I.mustEvalExpr goodExpr
                ) >>= (`shouldBe` val)

        it "should throw error if it gets a Nothing" $
            run extract (
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
                D.fname = funcName,
                D.params = [D.Decl paramName1 D.TypeString, D.Decl paramName2 D.TypeInt],
                D.returnType = D.TypeString,
                D.block = D.Block {
                    D.blockDecls = [],
                    D.blockStmts = [D.AssignStmt funcName (D.VarExpr paramName1)]
                    }
                }
            accessFirstArgReturnNone = D.Func {
                D.fname = funcName,
                D.params = [D.Decl paramName1 D.TypeString],
                D.returnType = D.TypeNone,
                D.block = D.Block {
                    D.blockDecls = [],
                    -- access arg here
                    D.blockStmts = [D.AssignStmt paramName1 (D.VarExpr paramName1)]
                    }
                }
            accessReturnButReturnNone = D.Func {
                D.fname = funcName,
                D.params = [],
                D.returnType = D.TypeNone,
                D.block = D.Block {
                    D.blockDecls = [],
                    -- should fail, since return value doesn't exist
                    D.blockStmts = [D.AssignStmt funcName (D.IntExpr 1)]
                    }
                }
            returnDefault = D.Func {
                D.fname = funcName,
                D.params = [],
                D.returnType = D.TypeInt,
                D.block = D.Block {
                    D.blockDecls = [],
                    D.blockStmts = []
                    }
                }
            accessParentScope = D.Func {
                D.fname = funcName,
                D.params = [D.Decl paramName1 D.TypeString],
                D.returnType = D.TypeNone,
                D.block = D.Block {
                    D.blockDecls = [],
                    D.blockStmts = [D.AssignStmt parentScopeVarName (D.VarExpr paramName1)]
                    }
                }
            duplicateDeclaration = D.Func {
                D.fname = funcName,
                D.params = [
                    D.Decl paramName1 D.TypeString,
                    D.Decl paramName1 D.TypeString
                    ],
                D.returnType = D.TypeNone,
                D.block = D.Block {
                    D.blockDecls = [],
                    D.blockStmts = []
                    }
                }
        it "should make arguments and return values accessible" $
            run extract (do
                -- function must be on the stack, to trigger conversion to return value
                S.declareVar funcName $ S.FuncValue returnFirstArg

                I.evalFuncValue returnFirstArg [v1, v2]
                ) >>= (pure . S.getValue . fromJust)
                  >>= (`shouldBe` v1)

        it "should return Nothing if no return value" $
            run extract (
                I.evalFuncValue accessFirstArgReturnNone [v1]
                ) >>= (`shouldBe` Nothing)

        it "shouldn't put return value on stack when return type is TypeNone" $
            run extract (
                I.evalFuncValue accessReturnButReturnNone []
                ) `shouldThrow` anyException -- illegal access

        it "should set return value to default value for type" $
            run extract (
                I.evalFuncValue returnDefault []
                ) >>= (`shouldBe` (Just $ S.IntValue 0))

        it "should allow changes to variables from global scope" $
            run extract (do
                S.overwrite parentScopeVarName namedValue1
                _ <- I.evalFuncValue accessParentScope [namedValue2]
                S.find parentScopeVarName
                ) >>= (`shouldBe` Just namedValue2)

        it "should not allow duplicate parameter names" $
            run extract (
                I.evalFuncValue duplicateDeclaration [namedValue1, namedValue2]
                ) `shouldThrow` anyException -- duplicate declaration

        it "should leave stack unchanged after execution" $ do
            (stack1, stack2) <- run extract (do
                S.declareVar funcName $ S.FuncValue returnFirstArg
                stack1 <- get
                _ <- I.evalFuncValue returnFirstArg [v1, v2]
                stack2 <- get
                return (stack1, stack2)
                )
            stack2 `shouldBe` stack1

    describe "xor" $
        it "should work in all possible cases" $ do
            I.xor True  True  `shouldBe` False
            I.xor True  False `shouldBe` True
            I.xor False True  `shouldBe` True
            I.xor False False `shouldBe` False

    describe "rvName" $
        it "should produce correct name for dummy function" $
            I.rvName mockFunc `shouldBe` D.Id ".retval$name1"

    describe "cast" $ do
        it "should work as identity on same type" $
            mapM_ (\v -> I.cast (S.typeOf v) v `shouldBe` Just v) mockValues

        it "should lift named value before cast" $
            mapM_ (\(v, nv) -> I.cast (S.typeOf v) nv `shouldBe` Just v) $ zip mockValues mockNamedValues

        it "should cast int to float" $
            I.cast D.TypeFloat (S.IntValue 2) `shouldBe` Just (S.FloatValue 2)

        it "should refuse to cast float to int" $
            I.cast D.TypeInt (S.FloatValue 2) `shouldBe` Nothing

        it "should refuse to cast int to bool" $
            I.cast D.TypeBool (S.IntValue 2) `shouldBe` Nothing

        it "should refuse to cast bool to int" $
            I.cast D.TypeInt (S.BoolValue False) `shouldBe` Nothing

    describe "mustCast" $ do
        it "should return raw result if cast was ok" $ do
            let val = S.BoolValue False
            I.mustCast D.TypeBool val `shouldBe` val

        it "should throw something if cast failed" $ do
            let val = S.BoolValue False
            evaluate (I.mustCast D.TypeFunc val) `shouldThrow` anyException

    describe "splitAtSpace" $ do
        it "should split at the first space on a normal string" $
            I.splitAtSpace "my string is cool" `shouldBe` ("my", "string is cool")

        it "should split at the first character if that's a space" $
            I.splitAtSpace " space is first" `shouldBe` ("", "space is first")

        it "should split on multiple spaces but doesn't trim" $
            I.splitAtSpace "many  spaces" `shouldBe` ("many", " spaces")

        it "should split empty string" $
            I.splitAtSpace "" `shouldBe` ("", "")

        it "should split singleton string" $
            I.splitAtSpace "a" `shouldBe` ("a", "")

        it "should split singleton space" $
            I.splitAtSpace " " `shouldBe` ("", "")

        it "should work if there are no spaces" $
            I.splitAtSpace "abcdef" `shouldBe` ("abcdef", "")

        it "should split if the only space is the last character" $
            I.splitAtSpace "abcdef " `shouldBe` ("abcdef", "")

    describe "isvar" $ do
        it "should return true if a named value" $
            I.isvar (S.NamedValue (D.Id "myfunc") $ S.FuncValue mockFunc) `shouldBe` True

        it "should return false if not a named value" $
            I.isvar (S.FuncValue mockFunc) `shouldBe` False

    describe "doesMatch" $ do
        it "should reject anything except NamedValue and IntValue" $ do
            let decl = makeCaseDecl [(1, 1)]
            mapM_ (\val ->
                evaluate (I.doesMatch val decl) `shouldThrow` anyException
                ) $ filter (not . isOfType D.TypeInt) mockValues

        it "should cast NamedValue to IntValue" $ do
            let decl = makeCaseDecl [(1, 1), (10, 10)]
            I.doesMatch (S.NamedValue (D.Id "myInt") $ S.IntValue 1) decl `shouldBe` True

        it "should doent cast NamedValue to anything besides IntValue" $ do
            let decl = makeCaseDecl [(1, 1)]
            mapM_ (\val ->
                evaluate (I.doesMatch val decl) `shouldThrow` anyException
                ) $ filter (not . isOfType D.TypeInt . S.getValue) mockNamedValues

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

            mapM_ (\(op, result) ->
                (I.combineToNum op n1 n2, op) `shouldBe` (result, op)
                ) $ zip ops results

        it "should throw on unknown operand" $
            evaluate (I.combineToNum "/" (1 :: Integer) (2 :: Integer)) `shouldThrow` anyException

    describe "combineToBool" $ do
        it "should apply '<>', '=', '>', '<', '<=', '>=' to operands" $ do
            let ops = ["<>", "=", ">", "<", "<=", ">="]
                results = [True, False, False, True, True, False]
                n1 = 1 :: Integer
                n2 = 2 :: Integer

            mapM_ (\(op, result) ->
                (I.combineToBool op n1 n2, op) `shouldBe` (result, op)
                ) $ zip ops results

        it "should throw on unknown operand" $
            evaluate (I.combineToBool "?" (1 :: Integer) (2 :: Integer)) `shouldThrow` anyException
