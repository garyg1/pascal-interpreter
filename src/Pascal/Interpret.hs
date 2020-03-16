module Pascal.Interpret where

import           Control.Exception
import           Control.Monad.State
import qualified Data.Map            as Map
import           Pascal.Data
import           Pascal.State        as S
import           System.IO

interpret :: Program -> S.AppState ()
interpret (Program _ block) = do
    liftIO $ do
        putStrLn "Hello, world!"
    visitBlock block

visitBlock :: Block -> S.AppState ()
visitBlock (Block decls stmts) = do
    mapM_ visitDecl decls
    mapM_ visitStmt stmts
    return ()

visitDecl :: Decl -> S.AppState ()
visitDecl decl = do
    case decl of
        VarDecls vds  -> mapM_ visitVarDecl vds
        FuncDecl func -> visitFuncDecl func
    return ()

visitVarDecl :: VarDecl -> S.AppState ()
visitVarDecl decl = do
    case decl of
        Decl name varType -> S.declare name $ S.valueOf varType
        _ -> do
            val <- evalExpr $ expr decl
            S.declare (name decl) $ must val

visitFuncDecl :: FuncOrProc -> S.AppState ()
visitFuncDecl func = S.declare (fname func) (S.FuncValue func)

visitStmt :: Stmt -> S.AppState ()
visitStmt stmt = case stmt of
    Stmts stmts -> mapM_ visitStmt stmts
    IfStmt ifExpr thenStmt -> visitStmt $ IfElseStmt ifExpr thenStmt (Stmts [])
    IfElseStmt ifExpr thenStmt elseStmt -> do
        val <- evalExpr ifExpr
        case val of
            Just (S.BoolValue True)  -> do visitStmt thenStmt
            Just (S.BoolValue False) -> do visitStmt elseStmt
            _                        -> throw $ S.IncorrectType "if condition" TypeBool $ must val
    WhileStmt whileExpr doStmt -> do
        val <- evalExpr whileExpr
        case val of
            Just (S.BoolValue True)  -> mapM_ visitStmt [doStmt, stmt]
            Just (S.BoolValue False) -> return ()
            _                        -> throw $ S.IncorrectType "while condition" TypeBool $ must val
    AssignStmt name expr -> do
        rhs <- evalExpr expr
        lhs <- S.mustFind name
        case (lhs, must rhs) of
            (FuncValue f, FuncValue g) -> S.mustReplace name (FuncValue g)
            (FuncValue f, rhs')        -> let rName = rvName f in S.mustReplace rName rhs'
            (_, rhs')                  -> S.mustReplace name rhs'
    CaseStmt case' cases -> visitStmt $ CaseElseStmt case' cases (Stmts [])
    CaseElseStmt case' cases elseStmt -> do
        val <- evalExpr case'
        case filter (doesMatch $ must val) cases of
            ((CaseDecl _ thenStmt) : _) -> visitStmt thenStmt
            []                          -> visitStmt elseStmt
    FuncCallStmt call -> do
        visitFuncCall call
        return ()
    _ -> throw S.NotImplemented

doesMatch :: S.Value -> CaseDecl -> Bool
doesMatch val (CaseDecl ranges _) = case val of
    S.IntValue i -> any (\(IntRange lo hi) -> (lo <= i) && (i <= hi)) ranges
    _            -> throw $ IncorrectType "case expression" TypeInt val

evalExpr :: Expr -> S.AppState (Maybe S.Value)
evalExpr expr = do
    case expr of
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
visitFuncCall (FuncCall fname exprs) = do
    defn <- S.find fname
    args <- mapM evalExpr exprs
    let defn' = must $ cast TypeFunc $ must defn
        func = S.getFunc defn'
        returnName = rvName func
        args' = map must args
        in do
            st <- get
            S.pushEmpty
            mapM_ (\(a, p) -> S.overwrite (name p) a) $ zip args' (params func)
            if (returnType func) /= TypeNone
                then S.overwrite returnName (valueOf $ returnType func)
                else return ()
            visitBlock (block func)
            rv <- S.find returnName
            S.pop
            return rv

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
        )
    (S.BoolValue b1, S.BoolValue b2) -> return $ S.BoolValue (
        case op of
            "and" -> b1 && b2
            "or"  -> b1 || b2
            "xor" -> xor b1 b2
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
    "<>" -> not $ n1 == n2
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
    (S.NamedValue _ val', _)       -> cast type' val'
    (S.BoolValue val', TypeBool)   -> Just val
    (S.IntValue val', TypeInt)     -> Just val
    (S.IntValue val', TypeFloat)   -> Just $ S.FloatValue $ fromIntegral val'
    (S.FloatValue val', TypeFloat) -> Just val
    (S.StrValue val', TypeString)  -> Just val
    (S.FuncValue val', TypeFunc)   -> Just val
    (_, _)                         -> Nothing

must :: Maybe a -> a
must mv = case mv of
    Just v -> v
    _      -> throw S.CannotEval
