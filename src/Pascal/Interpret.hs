module Pascal.Interpret where

import           Control.Exception
import           Control.Monad.Except
import           Pascal.Data
import           Pascal.State         as S

interpret :: Program -> S.AppState ()
interpret (Program _ bl) = do
    declareNativeFunctions
    visitBlock bl

declareNativeFunctions :: S.AppState ()
declareNativeFunctions = do
    S.overwrite (Id "sqrt") $ nativeFuncFrom sqrt
    S.overwrite (Id "sin") $ nativeFuncFrom sin
    S.overwrite (Id "cos") $ nativeFuncFrom cos
    S.overwrite (Id "exp") $ nativeFuncFrom exp
    S.overwrite (Id "ln") $ nativeFuncFrom log
    S.overwrite (Id "writeln") $ NativeFuncValue $ NativeFunc (\args -> liftIO $ do
        mapM_ (\arg -> print arg) args
        return Nothing
        )
    S.overwrite (Id "readln") $ NativeFuncValue $ NativeFunc (\args -> do
        if not $ all isvar args
            then throw $ S.VariableExpected "readln"
            else return ()
        foldM_ (\(line, shouldFlush) (S.NamedValue name val) -> do
            line' <- if shouldFlush then liftIO getLine else return line
            case S.typeOf val of
                TypeString -> do
                    S.overwrite name $ S.NamedValue name $ S.StrValue line'
                    return ("", True)
                type' -> do
                    S.overwrite name $ S.NamedValue name $ case type' of
                        TypeInt   -> S.IntValue $ read word
                        TypeFloat -> S.FloatValue $ read word
                        otherType -> throw $ CannotRead otherType
                    return (rest, False)
                    where (word, rest) = splitAtSpace line'
            ) ("", True) args
        return Nothing
        )

nativeFuncFrom :: (Float -> Float) -> S.Value
nativeFuncFrom fn = S.NativeFuncValue $ S.NativeFunc $ \(arg : rest) ->
    if length rest /= 0
        then throw $ S.IncorrectArgs (Id "sqrt") (arg : rest) [Decl (Id "operand") TypeInt]
        else let (S.FloatValue f) = must $ cast TypeFloat arg
            in return $ Just $ S.FloatValue $ fn f

visitBlock :: Block -> S.AppState ()
visitBlock (Block decls stmts) = do
    mapM_ visitDecl decls
    mapM_ visitStmt stmts
    return ()

visitDecl :: Decl -> S.AppState ()
visitDecl decl = do
    case decl of
        VarDecls vds   -> mapM_ (visitVarDecl False) vds
        ConstDecls cds -> mapM_ (visitVarDecl True) cds
        FuncDecl func  -> visitFuncDecl func
    return ()

visitVarDecl :: Bool -> VarDecl -> S.AppState ()
visitVarDecl isConst decl = do
    case decl of
        Decl name type' -> S.declare isConst name $ S.NamedValue name $ S.valueOf type'
        _ -> do
            val <- evalExpr $ expr decl
            S.declare isConst (dname decl) $ S.NamedValue (dname decl) $ must val

visitFuncDecl :: FuncOrProc -> S.AppState ()
visitFuncDecl func = S.declare False (fname func) (S.FuncValue func)

visitStmt :: Stmt -> S.AppState ()
visitStmt stmt = case stmt of
    AssignStmt name e -> do
        rhs <- evalExpr e
        lhs <- S.mustFind name
        case (lhs, must rhs) of
            (FuncValue _, FuncValue g) -> S.mustReplace name (FuncValue g)
            (FuncValue f, rhs')        -> let rName = rvName f in S.mustReplace rName rhs'
            (_, rhs')                  -> S.mustReplace name (S.NamedValue name rhs')

    BreakStmt -> throwError S.Break
    ContinueStmt -> throwError S.Continue

    CaseStmt case' cases -> visitStmt $ CaseElseStmt case' cases (Stmts [])
    CaseElseStmt case' cases elseStmt -> do
        val <- evalExpr case'
        case filter (doesMatch $ must val) cases of
            ((CaseDecl _ thenStmt) : _) -> visitStmt thenStmt
            []                          -> visitStmt elseStmt

    ForToStmt cvarName e1 e2 st -> visitForStmt True cvarName e1 e2 st
    ForDownToStmt cvarName e1 e2 st -> visitForStmt False cvarName e1 e2 st

    FuncCallStmt call -> do
        _ <- visitFuncCall call
        return ()

    IfStmt ifExpr thenStmt -> visitStmt $ IfElseStmt ifExpr thenStmt (Stmts [])
    IfElseStmt ifExpr thenStmt elseStmt -> do
        val <- evalExpr ifExpr
        case val of
            Just (S.BoolValue True)  -> do visitStmt thenStmt
            Just (S.BoolValue False) -> do visitStmt elseStmt
            _                        -> throw $ S.IncorrectType "if condition" TypeBool $ must val

    Stmts stmts -> mapM_ visitStmt stmts

    WhileStmt whileExpr doStmt -> do
        val <- evalExpr whileExpr
        case val of
            Just (S.BoolValue True)  -> do
                catchError (do
                    visitStmt doStmt)
                    (\_ -> throw $ S.InternalError "asdf")
                visitStmt stmt
            Just (S.BoolValue False) -> return ()
            _                        -> throw $ S.IncorrectType "while condition" TypeBool $ must val



-- TODO add break, continue
visitForStmt :: Bool -> Id -> Expr -> Expr -> Stmt -> S.AppState ()
visitForStmt isUp name start end doStmt = do
    startVal <- evalExpr start
    endVal <- evalExpr end
    _ <- S.mustFind name
    let startVal' = (S.getInt . must) startVal
        endVal' = (S.getInt . must) endVal
        in catchError (do
            mapM_ (\newval -> do
                S.mustReplace name $ S.NamedValue name $ S.IntValue newval
                catchError (do visitStmt doStmt) (\evt -> case evt of
                    S.Continue -> return ()
                    S.Break    -> throwError evt
                    )
                ) $ if isUp then [startVal' .. endVal'] else reverse [endVal' .. startVal']

            S.mustReplace name $ S.NamedValue name $ S.IntValue endVal'
            ) (\evt -> case evt of
                S.Break -> return ()
                _       -> throw $ S.InternalError "unknown error thrown"
            )
    S.setConst False name

evalExpr :: Expr -> S.AppState (Maybe S.Value)
evalExpr e = do
    case e of
        VarExpr name        -> S.find name
        IntExpr i           -> return $ Just $ S.IntValue i
        BoolExpr b          -> return $ Just $ S.BoolValue b
        StrExpr s           -> return $ Just $ S.StrValue s
        FltExpr f           -> return $ Just $ S.FloatValue f
        BinaryExpr op b1 b2 -> do
            v1 <- evalExpr b1
            v2 <- evalExpr b2
            v3 <- combine op (must v1) (must v2)
            return $ Just v3
        FuncCallExpr call   -> visitFuncCall call
        _                   -> throw S.NotImplemented

visitFuncCall :: FuncCall -> S.AppState (Maybe S.Value)
visitFuncCall (FuncCall name exprs) = do
    defn <- S.find name
    args <- mapM evalExpr exprs
    case must defn of
        S.NativeFuncValue (NativeFunc fn) -> fn $ map must args
        S.FuncValue func -> let
            returnName = rvName func
            args' = map must args
            in do
                S.pushEmpty
                mapM_ (\(a, p) -> S.overwrite (dname p) a) $ zip args' (params func)
                if (returnType func) /= TypeNone
                    then S.overwrite returnName (valueOf $ returnType func)
                    else return ()
                visitBlock (block func)
                rv <- S.find returnName
                S.pop
                return rv
        _ -> throw S.NotImplemented

combine :: String -> S.Value -> S.Value -> S.AppState (S.Value)
combine op (S.NamedValue _ v1') v2 = combine op v1' v2
combine op v1 (S.NamedValue _ v2') = combine op v1 v2'
combine op v1 v2 = case (v1, v2) of
    (S.IntValue i1, S.IntValue i2) -> return (
        case op of
            "/" -> S.IntValue $ div i1 i2
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
            _   -> throw $ S.InternalError "unknown operation on strings"
        )
    (S.BoolValue b1, S.BoolValue b2) -> return $ S.BoolValue (
        case op of
            "and" -> b1 && b2
            "or"  -> b1 || b2
            "xor" -> xor b1 b2
            _     -> throw $ S.InternalError "unknown operation on bools"
        )
    _ -> do
        t1' <- marshal (v1, v2)
        case t1' of
            Just (v1', v2') -> combine op v1' v2'
            Nothing         -> throw S.CannotCombine

combineToNum :: (Num n) => String -> n -> n -> n
combineToNum op n1 n2 = case op of
    "+" -> n1 + n2
    "*" -> n1 * n2
    "-" -> n1 - n2
    _   -> throw CannotCombine

combineToBool :: (Ord n, Eq n) => String -> n -> n -> Bool
combineToBool op n1 n2 = case op of
    "<>" -> n1 /= n2
    "="  -> n1 == n2
    ">"  -> n1 > n2
    "<"  -> n1 < n2
    "<=" -> n1 <= n2
    ">=" -> n1 <= n2
    _    -> throw CannotCombine

marshal :: (S.Value, S.Value) -> S.AppState (Maybe (S.Value, S.Value))
marshal (v1, v2) = case (v1, v2) of
    (S.FuncValue f, _) -> do
        v1' <- S.find $ rvName f
        marshal (must v1', v2)
    (_, S.FuncValue g) -> do
        v2' <- S.find $ rvName g
        marshal (v1, must v2')

    (S.IntValue _, S.FloatValue _) -> return $ Just (must (cast TypeFloat v1), v2)
    (S.FloatValue _, S.IntValue _) -> return $ Just (v1, must (cast TypeFloat v2))

    (_, _) -> return Nothing

-- improves on https://annevankesteren.nl/2007/02/haskell-xor
xor :: Bool -> Bool -> Bool
xor True a = not a
xor _ a    = a

rvName :: FuncOrProc -> Id
rvName f = (Id $ ".retval" ++ (toString . fname $ f))

cast :: PascalType -> S.Value -> Maybe S.Value
cast type' val = case (val, type') of
    (S.NamedValue _ val', _)     -> cast type' val'
    (S.BoolValue _, TypeBool)    -> Just val
    (S.IntValue _, TypeInt)      -> Just val
    (S.IntValue val', TypeFloat) -> Just $ S.FloatValue $ fromIntegral val'
    (S.FloatValue _, TypeFloat)  -> Just val
    (S.StrValue _, TypeString)   -> Just val
    (S.FuncValue _, TypeFunc)    -> Just val
    (_, _)                       -> Nothing

must :: Maybe a -> a
must mv = case mv of
    Just v -> v
    _      -> throw $ S.InternalError "Undefined Symbol"

-- partially inspired by https://stackoverflow.com/q/40297001/8887313
splitAtSpace :: [Char] -> ([Char], [Char])
splitAtSpace (' ' : after) = ([], after)
splitAtSpace (x : xs) = let
    (rest, after) = splitAtSpace xs
    in (x : rest, after)
splitAtSpace [] = ([], [])

isvar :: S.Value -> Bool
isvar (S.NamedValue _ _) = True
isvar _                  = False

doesMatch :: S.Value -> CaseDecl -> Bool
doesMatch val range = let
    (CaseDecl ranges _) = range
    in case val of
        S.NamedValue _ v -> doesMatch v range
        S.IntValue i -> any (\(IntRange lo hi) -> (lo <= i) && (i <= hi)) ranges
        _            -> throw $ IncorrectType "case expression" TypeInt val
