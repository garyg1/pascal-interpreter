module Pascal.Interpret where

import           Control.Exception
import           Control.Monad.Except
import           Pascal.Data
import qualified Pascal.State         as S

interpret :: Program -> S.AppState ()
interpret (Program _ bl) = do
    declareNativeFunctions
    visitBlock bl

visitBlock :: Block -> S.AppState ()
visitBlock (Block decls stmts) = do
    mapM_ visitDecl decls
    mapM_ visitStmt stmts

visitDecl :: Decl -> S.AppState ()
visitDecl decl = case decl of
    VarDecls vds   -> mapM_ (visitVarDecl False) vds
    ConstDecls cds -> mapM_ (visitVarDecl True) cds
    FuncDecl func  -> visitFuncDecl func

visitVarDecl :: Bool -> VarDecl -> S.AppState ()
visitVarDecl isConst decl = do
    let name = dname decl
    val <- case decl of
        Decl _ type' -> return $ S.valueOf type'
        DeclTypeDefn _ type' expr' -> do
            val' <- mustEvalExpr expr'
            return $ mustCast type' val'
        DeclDefn _ expr' -> mustEvalExpr expr'
    S.declare isConst name $ S.NamedValue name val

visitFuncDecl :: Func -> S.AppState ()
visitFuncDecl func = S.declareVar (fname func) (S.FuncValue func)

visitStmt :: Stmt -> S.AppState ()
visitStmt stmt = case stmt of
    AssignStmt name e -> visitAssignStmt name e
    BreakStmt -> throwError S.Break
    ContinueStmt -> throwError S.Continue
    CaseStmt caseExpr cases -> visitCaseElseStmt caseExpr cases (Stmts [])
    CaseElseStmt caseExpr cases elseStmt -> visitCaseElseStmt caseExpr cases elseStmt
    ForDownToStmt cvarName e1 e2 st -> visitForStmt False cvarName e1 e2 st
    ForToStmt cvarName e1 e2 st -> visitForStmt True cvarName e1 e2 st
    FuncCallStmt call -> do
        _ <- evalFuncCall call
        return ()
    IfElseStmt i t e -> visitIfElseStmt i t e
    IfStmt ifExpr thenStmt -> visitIfElseStmt ifExpr thenStmt (Stmts [])
    Stmts stmts -> mapM_ visitStmt stmts
    WhileStmt _ _ -> catchError (visitWhileStmt stmt) (\case
            S.Break    -> return ()
            S.Continue -> throw $ S.InternalError "unknown error 'continue' thrown"
        )

visitAssignStmt :: Id -> Expr -> S.AppState ()
visitAssignStmt name e = do
    rhs <- mustEvalExpr e
    lhs <- S.mustFind name
    case (lhs, rhs) of
        (S.FuncValue _, S.FuncValue _)             -> S.mustReplace name rhs
        (S.FuncValue f, _)                         -> visitAssignStmt (rvName f) e
        (S.NativeFuncValue _, S.NativeFuncValue _) -> S.mustReplace name rhs
        (_, _)                                     -> S.mustReplace name $ S.NamedValue name $
                                                        mustCast (S.typeOf lhs) rhs

visitCaseElseStmt :: Expr -> [CaseDecl] -> Stmt -> S.AppState ()
visitCaseElseStmt _ [] _ = throw $ S.InternalError "Unexpected empty case statement"
visitCaseElseStmt caseExpr cases elseStmt = do
    val <- mustEvalExpr caseExpr
    case filter (doesMatch val) cases of
        ((CaseDecl _ thenStmt) : _) -> visitStmt thenStmt
        []                          -> visitStmt elseStmt

visitForStmt :: Bool -> Id -> Expr -> Expr -> Stmt -> S.AppState ()
visitForStmt isUp name start end doStmt = do
    startVal <- mustEvalExpr start
    endVal <- mustEvalExpr end
    let startVal' = S.getInt startVal
        endVal' = S.getInt endVal

    iterationVal <- S.mustFind name
    case S.typeOf iterationVal of
        TypeInt -> return ()
        _       -> throw $ S.IncorrectType "for iteration" TypeInt iterationVal

    isConst <- S.isConst name
    case isConst of
        Nothing   -> throw $ S.UndeclaredSymbol name
        Just True -> throw $ S.CannotBeConst name
        _         -> return ()

    catchError (do
        mapM_ (\newval -> do
            setIterationVar name newval
            catchError (visitStmt doStmt) (\evt -> case evt of
                S.Continue -> return ()
                S.Break    -> throwError evt
                )
            ) $ if isUp
                then [startVal' .. endVal']
                else reverse [endVal' .. startVal']

        setIterationVar name endVal'
        ) (\case
            S.Break -> return ()
            _       -> throw $ S.InternalError "unknown error thrown"
            )

    -- this is OK, since `name` was definitely VAR before it was loop variable
    S.setConst False name

visitIfElseStmt :: Expr -> Stmt -> Stmt -> S.AppState ()
visitIfElseStmt ifExpr thenStmt elseStmt = do
    val <- mustEvalExpr ifExpr
    case cast TypeBool val of
        Just (S.BoolValue True)  -> visitStmt thenStmt
        Just (S.BoolValue False) -> visitStmt elseStmt
        _                        -> throw $ S.IncorrectType "if condition" TypeBool val

visitWhileStmt :: Stmt -> S.AppState ()
visitWhileStmt whileStmt = do
    let WhileStmt whileExpr doStmt = whileStmt
    val <- mustEvalExpr whileExpr
    case cast TypeBool val of
        Just (S.BoolValue True) -> do
            catchError (visitStmt doStmt) (\evt -> case evt of
                S.Continue -> return ()
                S.Break    -> throwError evt
                )
            visitWhileStmt whileStmt
        Just (S.BoolValue False) -> return ()
        _                        -> throw $ S.IncorrectType "while condition" TypeBool val

setIterationVar :: Id -> Int -> S.AppState ()
setIterationVar name newVal = do
    S.setConst False name
    S.mustReplace name $ S.NamedValue name $ S.IntValue newVal
    S.setConst True name

mustEvalExpr :: Expr -> S.AppState S.Value
mustEvalExpr e = do
    val <- evalExpr e
    case val of
        Just val' -> return val'
        Nothing   -> throw $ S.CannotEval e

evalExpr :: Expr -> S.AppState (Maybe S.Value)
evalExpr e = case e of
    VarExpr name        -> S.find name
    FuncCallExpr call   -> evalFuncCall call
    IntExpr i           -> return $ Just $ S.IntValue i
    BoolExpr b          -> return $ Just $ S.BoolValue b
    StrExpr s           -> return $ Just $ S.StrValue s
    FltExpr f           -> return $ Just $ S.FloatValue f
    BinaryExpr op b1 b2 -> do
        v1 <- evalExpr b1
        v2 <- evalExpr b2
        case (v1, v2) of
            (Nothing, _) -> return Nothing
            (_, Nothing) -> return Nothing
            (Just v1', Just v2') -> do
                v3 <- combine op v1' v2'
                return $ Just v3
    _                   -> throw S.NotImplemented

evalFuncCall :: FuncCall -> S.AppState (Maybe S.Value)
evalFuncCall (FuncCall name exprs) = do
    defn <- S.mustFind name
    args <- mapM mustEvalExpr exprs
    case defn of
        S.NativeFuncValue (S.NativeFunc _ fn) -> fn args
        S.FuncValue func                      -> evalFuncValue func args
        _                                     -> throw S.NotImplemented

evalFuncValue :: Func -> [S.Value] -> S.AppState (Maybe S.Value)
evalFuncValue func args = do
    let returnName = rvName func
    S.pushEmpty
    mapM_ (\(a, p) -> S.declareVar (dname p) a) $ zip args (params func)
    when (returnType func /= TypeNone) $
        S.overwrite returnName (S.valueOf $ returnType func)
    visitBlock (block func)
    rv <- S.find returnName
    S.pop
    return rv

declareNativeFunctions :: S.AppState ()
declareNativeFunctions = do
    S.declareVar (Id "sqrt") $ nativeFuncFrom (Id "sqrt") sqrt
    S.declareVar (Id "sin") $ nativeFuncFrom (Id "sin") sin
    S.declareVar (Id "cos") $ nativeFuncFrom (Id "cos") cos
    S.declareVar (Id "exp") $ nativeFuncFrom (Id "exp") exp
    S.declareVar (Id "ln") $ nativeFuncFrom (Id "ln") log
    S.declareVar (Id "writeln") $ S.NativeFuncValue $ S.NativeFunc (Id "writeln") writeln
    S.declareVar (Id "readln") $ S.NativeFuncValue $ S.NativeFunc (Id "readln") readln

readln :: [S.Value] -> S.AppState (Maybe S.Value)
readln args = do
    unless (all isvar args) $
        throw $ S.VariableExpected "readln"
    foldM_ (\(line, shouldFlush) (S.NamedValue name val) -> do
        line' <- if shouldFlush then liftIO getLine else return line
        case S.typeOf val of
            TypeString -> do
                S.overwrite name $ S.NamedValue name $ S.StrValue line'
                return ("", True)
            type' -> do
                let (word, rest) = splitAtSpace line'
                S.overwrite name $ S.NamedValue name $ case type' of
                    TypeInt   -> S.IntValue $ read word
                    TypeFloat -> S.FloatValue $ read word
                    otherType -> throw $ S.CannotRead otherType
                return (rest, False)
        ) ("", True) args
    return Nothing

writeln :: [S.Value] -> S.AppState (Maybe S.Value)
writeln args = liftIO $ do
    mapM_ (putStr . show) args
    putStrLn ""
    return Nothing

nativeFuncFrom :: Id -> (Float -> Float) -> S.Value
nativeFuncFrom name fn = S.NativeFuncValue $ S.NativeFunc name $ \(arg : rest) ->
    if null rest
        then let (S.FloatValue f) = mustCast TypeFloat arg
            in return $ Just $ S.FloatValue $ fn f
        else throw $ S.IncorrectArgs name (arg : rest) [Decl (Id "operand") TypeInt]

combine :: String -> S.Value -> S.Value -> S.AppState S.Value
combine op (S.NamedValue _ v1') v2 = combine op v1' v2
combine op v1 (S.NamedValue _ v2') = combine op v1 v2'
combine op v1 v2 = case (v1, v2) of
    (S.IntValue i1, S.IntValue i2) -> return (
        case op of
            "/" -> S.IntValue $ div i1 i2
            "mod" -> S.IntValue $ mod i1 i2
            _   | op `elem` ["+", "*", "-"] -> S.IntValue $ combineToNum op i1 i2
                | otherwise                 -> S.BoolValue $ combineToBool op i1 i2
        )
    (S.FloatValue f1, S.FloatValue f2) -> return (
        case op of
            "/" -> S.FloatValue $ f1 / f2
            _   | op `elem` ["+", "*", "-", "/"] -> S.FloatValue $ combineToNum op f1 f2
                | otherwise                      -> S.BoolValue $ combineToBool op f1 f2
        )
    (S.StrValue s1, S.StrValue s2) -> return (
        case op of
            "+" -> S.StrValue $ s1 ++ s2
            _   -> S.BoolValue $ combineToBool op s1 s2
        )
    (S.BoolValue b1, S.BoolValue b2) -> return $ S.BoolValue (
        case op of
            "and" -> b1 && b2
            "or"  -> b1 || b2
            "xor" -> xor b1 b2
            _     -> combineToBool op b1 b2
        )
    _ -> do
        t1' <- marshal (v1, v2)
        case t1' of
            Just (v1', v2') -> combine op v1' v2'
            Nothing         -> throw $ S.CannotCombine (show v1) (show v2)

combineToNum :: (Show n, Num n) => String -> n -> n -> n
combineToNum op n1 n2 = case op of
    "+" -> n1 + n2
    "*" -> n1 * n2
    "-" -> n1 - n2
    _   -> throw $ S.CannotCombine (show n1) (show n2)

combineToBool :: (Show n, Ord n, Eq n) => String -> n -> n -> Bool
combineToBool op n1 n2 = case op of
    "<>" -> n1 /= n2
    "="  -> n1 == n2
    ">"  -> n1 > n2
    "<"  -> n1 < n2
    "<=" -> n1 <= n2
    ">=" -> n1 >= n2
    _    -> throw $ S.CannotCombine (show n1) (show n2)

marshal :: (S.Value, S.Value) -> S.AppState (Maybe (S.Value, S.Value))
marshal (v1, v2) = case (v1, v2) of
    (S.FuncValue f, _) -> do
        v1' <- S.mustFind $ rvName f
        marshal (v1', v2)
    (_, S.FuncValue g) -> do
        v2' <- S.mustFind $ rvName g
        marshal (v1, v2')

    (S.IntValue _, S.FloatValue _) -> return $ Just (mustCast TypeFloat v1, v2)
    (S.FloatValue _, S.IntValue _) -> return $ Just (v1, mustCast TypeFloat v2)

    (_, _) -> return Nothing

-- https://annevankesteren.nl/2007/02/haskell-xor
xor :: Bool -> Bool -> Bool
xor True a  = not a
xor False a = a

rvName :: Func -> Id
rvName f = Id $ ".retval$" ++ (toString . fname $ f)

cast :: PascalType -> S.Value -> Maybe S.Value
cast type' val = case (val, type') of
    (S.NamedValue _ val', _)              -> cast type' val'
    (S.BoolValue _, TypeBool)             -> Just val
    (S.IntValue _, TypeInt)               -> Just val
    (S.IntValue val', TypeFloat)          -> Just $ S.FloatValue $ fromIntegral val'
    (S.FloatValue _, TypeFloat)           -> Just val
    (S.StrValue _, TypeString)            -> Just val
    (S.FuncValue _, TypeFunc)             -> Just val
    (S.NativeFuncValue _, TypeNativeFunc) -> Just val
    (_, _)                                -> Nothing

mustCast :: PascalType -> S.Value -> S.Value
mustCast t v = case cast t v of
    Just v' -> v'
    Nothing -> throw $ S.CannotCast t v

-- partially inspired by https://stackoverflow.com/q/40297001/8887313
splitAtSpace :: String -> (String, String)
splitAtSpace (' ' : after) = ([], after)
splitAtSpace (x : xs) = let
    (rest, after) = splitAtSpace xs
    in (x : rest, after)
splitAtSpace [] = ([], [])

isvar :: S.Value -> Bool
isvar (S.NamedValue _ _) = True
isvar _                  = False

doesMatch :: S.Value -> CaseDecl -> Bool
doesMatch (S.NamedValue _ v) range = doesMatch v range
doesMatch (S.IntValue i) (CaseDecl ranges _) = any (\(IntRange lo hi) -> (lo <= i) && (i <= hi)) ranges
doesMatch val _ = throw $ S.IncorrectType "case expression" TypeInt val
