module Pascal.Interpret where

import           Control.Exception
import qualified Data.Map          as Map
import           Pascal.Data
import           Pascal.State      as State

interpret :: Program -> String
interpret (Program _ block) = show $ evalBlock State.new block

valueOf :: PascalType -> State.Value
valueOf TypeBool   = State.BoolValue False
valueOf TypeInt    = State.IntValue 0
valueOf TypeFloat  = State.FloatValue 0.0
valueOf TypeString = State.StrValue ""
valueOf _          = throw NotImplemented

eval :: State.PState -> Expr -> (Maybe State.Value, State.PState)
eval state expr = case expr of
    VarExpr name -> (Just (State.NamedValue name $ State.mustFind state name), state)
    IntExpr i -> (Just (State.IntValue i), state)
    StrExpr s -> (Just (State.StrValue s), state)
    FltExpr f -> (Just (State.FloatValue f), state)
    BoolExpr b -> (Just (State.BoolValue b), state)
    FuncCallExpr f -> evalFuncCall state f
    BinaryExpr op b1 b2 -> let
        (v1, state') = eval state b1
        (v2, state'') = eval state' b2
        in (Just (combine state op v1 v2), state)

mustEval :: State.PState -> Expr -> (State.Value, State.PState)
mustEval state expr = case eval state expr of
    (Nothing, _)     -> throw CannotEval
    (Just v, state') -> (v, state')

foldEval :: ([State.Value], State.PState) -> Expr -> ([State.Value], State.PState)
foldEval (vals, s) e = let (val, s') = mustEval s e in ((val : vals), s')

combine :: State.PState -> String -> Maybe State.Value -> Maybe State.Value -> State.Value
combine _ _ Nothing _                = throw CannotCombine
combine _ _ _ Nothing                = throw CannotCombine
combine state op (Just v1) (Just v2) = combine' state op v1 v2

combine' :: State.PState -> String -> State.Value -> State.Value -> State.Value
combine' state op (State.NamedValue _ v1') v2 = combine' state op v1' v2
combine' state op v1 (State.NamedValue _ v2') = combine' state op v1 v2'
combine' state op v1 v2 = case (v1, v2) of
    (State.IntValue i1, State.IntValue i2) -> case op of
        "+" -> State.IntValue $ i1 + i2
        "*" -> State.IntValue $ i1 * i2
        "-" -> State.IntValue $ i1 - i2
        "/" -> State.IntValue $ div i1 i2
        _   -> State.BoolValue $ combineToBool op i1 i2
    (State.FloatValue f1, State.FloatValue f2) -> case op of
        "+" -> State.FloatValue $ f1 + f2
        "*" -> State.FloatValue $ f1 * f2
        "-" -> State.FloatValue $ f1 - f2
        "/" -> State.FloatValue $ f1 / f2
        _   -> State.BoolValue $ combineToBool op f1 f2
    (State.StrValue s1, State.StrValue s2) -> case op of
        "+" -> State.StrValue $ s1 ++ s2
    (State.BoolValue b1, State.BoolValue b2) -> case op of
        "and" -> State.BoolValue $ b1 && b2
        "or"  -> State.BoolValue $ b1 || b2
        "xor" -> State.BoolValue $ xor b1 b2
    (_, _) -> case marshal state (v1, v2) of
        Just (v1', v2') -> combine' state op v1' v2'
        Nothing         -> throw CannotCombine

combineToBool :: (Ord n, Eq n) => String -> n -> n -> Bool
combineToBool op n1 n2 = case op of
    "<>" -> not $ n1 == n2
    ">"  -> n1 > n2
    "<"  -> n1 < n2
    "<=" -> n1 <= n2
    ">=" -> n1 <= n2
    _    -> throw CannotCombine

marshal :: State.PState -> (State.Value, State.Value) -> Maybe (State.Value, State.Value)
marshal state (v1, v2) = case (v1, v2) of
    (State.IntValue _, State.FloatValue _) -> Just (mustCast TypeFloat v1, v2)
    (State.FloatValue _, State.IntValue _) -> Just (v1, mustCast TypeFloat v2)
    (State.FuncValue f, _)                 -> let
        retName = Id $ ".retval" ++ (toString . fname $ f)
        in marshal state (State.mustFind state retName, v2)
    (_, State.FuncValue f)                 -> let
        retName = Id $ ".retval" ++ (toString . fname $ f)
        in marshal state (v1, State.mustFind state retName)
    (_, _) -> Nothing

-- improved version of https://annevankesteren.nl/2007/02/haskell-xor
xor :: Bool -> Bool -> Bool
xor True a = not a
xor _ a    = a

evalFuncCall :: State.PState -> FuncCall -> (Maybe State.Value, State.PState)
evalFuncCall state (FuncCall name args) = case State.mustFind state name of
    State.FuncValue funcOrProc -> evalFuncCall' state funcOrProc args
    val                        -> throw $ IncorrectType name "function" val

evalFuncCall' :: State.PState -> FuncOrProc -> [Expr] -> (Maybe State.Value, State.PState)
evalFuncCall' state (Func name params rType block) args = do
    let (args', state') = foldl foldEval ([], state) $ reverse args
        retName      = Id $ ".retval" ++ (toString name)
        innerState   = State.PState (prepArgs name args' params) (global state')
        innerState'  = State.overwrite innerState retName (State.NamedValue retName $ valueOf rType)
        innerState'' = evalBlock innerState' block
        state''      = (State.PState (stack state') (global innerState))
        retVal      = State.find innerState'' retName
        in (retVal, state'')
evalFuncCall' state (Proc name params block) args = throw NotImplemented

prepArgs :: Id -> [State.Value] -> [VarDecl] -> [State.Scope]
prepArgs name args params = case length args == length params of
    False -> throw $ IncorrectArgs name args params
    _ ->
        let args' = map mustCastArg $ zip args params
            scope = foldl (\m (State.NamedValue id val) -> Map.insert id val m) Map.empty args'
            in [scope]

mustCastArg :: (State.Value, VarDecl) -> State.Value
mustCastArg (val, decl) = let
    name' = name decl
    type' = varType decl
    val' = cast type' val
    in case val' of
        Just v  -> State.NamedValue name' v
        Nothing -> throw $ IncorrectType name' (show type') val

mustCast :: PascalType -> State.Value -> State.Value
mustCast type' val = case cast type' val of
    Just v  -> v
    Nothing -> throw $ IncorrectType (Id "unknown") (show type') val

cast :: PascalType -> State.Value -> Maybe State.Value
cast type' val = case (val, type') of
    (State.NamedValue _ val', _)       -> cast type' val'
    (State.BoolValue val', TypeBool)   -> Just $ State.BoolValue val'
    (State.IntValue val', TypeInt)     -> Just $ State.IntValue val'
    (State.IntValue val', TypeFloat)   -> Just $ State.FloatValue $ fromIntegral val'
    (State.FloatValue val', TypeFloat) -> Just $ State.FloatValue val'
    (State.StrValue val', TypeString)  -> Just $ State.StrValue val'
    (_, _) -> Nothing

evalBlock :: State.PState -> Block -> State.PState
evalBlock state (Block decls stmts) = do
    let state' = foldl evalDecls state decls
        in foldl evalStmt state' stmts

evalDecls :: State.PState -> Decl -> State.PState
evalDecls state decls = case decls of
    VarDecls vs -> foldl evalVarDecl state vs
    FuncDecl f  -> State.overwrite state (fname f) (FuncValue f)
    _           -> throw NotImplemented

evalVarDecl :: State.PState -> VarDecl -> State.PState
evalVarDecl state (Decl name t) = State.overwrite state name $ valueOf t
evalVarDecl state (DeclTypeDefn name t expr) = case eval state expr of
    (Nothing, _)     -> throw CannotCombine
    (Just v, state') -> State.overwrite state' name v
evalVarDecl state (DeclDefn name expr) = case eval state expr of
    (Nothing, _)     -> throw CannotCombine
    (Just v, state') -> State.overwrite state' name v

evalStmt :: State.PState -> Stmt -> State.PState
evalStmt state stmt = case stmt of
    Stmts stmts -> foldl evalStmt state stmts

    IfStmt ifExpr thenStmt -> evalStmt state $ IfElseStmt ifExpr thenStmt (Stmts [])
    IfElseStmt ifExpr thenStmt elseStmt -> case eval state ifExpr of
        (Just (State.BoolValue b), state') -> case b of
            True  -> evalStmt state' thenStmt
            False -> evalStmt state' elseStmt
        (Just val, _) -> throw $ IncorrectType (Id "if expression") "Bool" val
        (Nothing, _) -> throw CannotCombine

    AssignStmt name expr -> let
        (val, state') = mustEval state expr
        valToAssign = case val of
            NamedValue _ inner -> inner
            _                  -> val
        in case State.mustFind state' name of
            (FuncValue _) -> case valToAssign of
                -- assign func to func
                FuncValue _ -> State.replace name valToAssign state'
                _ -> -- assign value to return value
                    let retName = Id $ ".retval" ++ (toString name)
                    in State.replace retName (NamedValue retName valToAssign) state'
            _ -> State.replace name (NamedValue name valToAssign) state'

    WhileStmt expr whileStmt -> case eval state expr of
        (Just (State.BoolValue b), state') -> case b of
            True -> let
                state'' = evalStmt state' whileStmt
                in evalStmt state'' stmt
            False -> state'
        (Just val, _) -> throw $ IncorrectType (Id "while expression") "Bool" val
        (Nothing, _) -> throw CannotCombine

    FuncCallStmt call -> let (_, state') = evalFuncCall state call in state'

