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

eval :: State.State -> Expr -> (Maybe State.Value, State.State)
eval state expr = case expr of
    VarExpr name -> (Just (State.mustFind state name), state)
    IntExpr i -> (Just (State.IntValue i), state)
    StrExpr s -> (Just (State.StrValue s), state)
    FltExpr f -> (Just (State.FloatValue f), state)
    BoolExpr b -> (Just (State.BoolValue b), state)
    FuncCallExpr f -> evalFuncCall state f
    BinaryExpr op b1 b2 -> let
        (v1, state') = eval state b1
        (v2, state'') = eval state' b2
        in (Just (combine op v1 v2), state)

-- helpers for eval
mustEval :: State.State -> Expr -> (State.Value, State.State)
mustEval state expr = case eval state expr of
    (Nothing, _)     -> throw CannotEval
    (Just v, state') -> (v, state')

foldEval :: ([State.Value], State.State) -> Expr -> ([State.Value], State.State)
foldEval (vals, s) e = let (val, s') = mustEval s e in ((val : vals), s')

combine :: String -> Maybe State.Value -> Maybe State.Value -> State.Value
combine _ Nothing _            = throw CannotCombine
combine _ _ Nothing            = throw CannotCombine
combine op (Just v1) (Just v2) = combine' op v1 v2

combine' :: String -> State.Value -> State.Value -> State.Value
combine' op (State.NamedValue _ v1') v2 = combine' op v1' v2
combine' op v1 (State.NamedValue _ v2') = combine' op v1 v2'
combine' op v1 v2 = case (v1, v2) of
    (State.IntValue i1, State.IntValue i2) -> State.IntValue $ i1 + i2

evalFuncCall :: State.State -> FuncCall -> (Maybe State.Value, State.State)
evalFuncCall state (FuncCall name args) = case State.mustFind state name of
    State.FuncValue funcOrProc -> evalFuncCall' state funcOrProc args
    val                        -> throw $ IncorrectType name "function" val

evalFuncCall' :: State.State -> FuncOrProc -> [Expr] -> (Maybe State.Value, State.State)
evalFuncCall' state (Func name params rType block) args = do
    let (args', state') = foldl foldEval ([], state) $ reverse args
        innerState = evalBlock (State.State (prepArgs name args' params) (global state')) block
        state'' = (State.State (stack state') (global innerState))
        in (Nothing, state'')
evalFuncCall' state (Proc name params block) args = throw NotImplemented

prepArgs :: Id -> [State.Value] -> [VarDecl] -> [State.Scope]
prepArgs name args params = case length args == length params of
    False -> throw $ IncorrectArgs name args params
    _ ->
        let args' = map mustCastArg $ zip args params
            scope = foldl (\m v -> case v of
                State.NamedValue id val -> Map.insert id val m
                _ -> throw $ InternalError "Expected State.NamedValue from castArg"
                ) Map.empty args'
            in [scope]

mustCastArg :: (State.Value, VarDecl) -> State.Value
mustCastArg (val, decl) = let
    name' = (name decl)
    in case (val, varType decl) of
        (State.NamedValue _ val', _)       -> mustCastArg (val', decl)
        (State.BoolValue val', TypeBool)   -> State.NamedValue name' $ State.BoolValue val'
        (State.IntValue val', TypeInt)     -> State.NamedValue name' $ State.IntValue val'
        (State.FloatValue val', TypeFloat) -> State.NamedValue name' $ State.FloatValue val'
        (State.StrValue val', TypeString)  -> State.NamedValue name' $ State.StrValue val'
        (State.IntValue val', TypeFloat)   -> State.NamedValue name' $ State.FloatValue $ fromIntegral val'
        (val', type')                      -> throw $ IncorrectType name' (show type') val'

evalBlock :: State.State -> Block -> State.State
evalBlock state (Block decls stmts) = do
    let state' = foldl evalDecls state decls
        in foldl evalStmt state' stmts

evalDecls :: State.State -> Decl -> State.State
evalDecls state decls = case decls of
    VarDecls vs -> foldl evalVarDecl state vs
    FuncDecl f  -> State.put state (fname f) (FuncValue f)
    _           -> throw NotImplemented

evalVarDecl :: State.State -> VarDecl -> State.State
evalVarDecl state (Decl name t) = State.put state name $ valueOf t
evalVarDecl state (DeclTypeDefn name t expr) = case eval state expr of
    (Nothing, _)     -> throw CannotCombine
    (Just v, state') -> State.put state' name v
evalVarDecl state (DeclDefn name expr) = case eval state expr of
    (Nothing, _)     -> throw CannotCombine
    (Just v, state') -> State.put state' name v

evalStmt :: State.State -> Stmt -> State.State
evalStmt state stmt = case stmt of
    Stmts stmts -> foldl evalStmt state stmts

    IfStmt ifExpr thenStmt -> evalStmt state $ IfElseStmt ifExpr thenStmt (Stmts [])
    IfElseStmt ifExpr thenStmt elseStmt -> case eval state ifExpr of
        (Just (State.BoolValue b), state') -> case b of
            True  -> evalStmt state' thenStmt
            False -> evalStmt state' elseStmt
        (Just val, _) -> throw $ IncorrectType (Id "if expression") "Bool" val
        (Nothing, _) -> throw CannotCombine

    AssignStmt name expr -> case eval state expr of
        (Just val, state') -> State.replace name val state'
        (Nothing, _)       -> throw CannotCombine

    WhileStmt expr whileStmt -> case eval state expr of
        (Just (State.BoolValue b), state') -> case b of
            True -> let
                state'' = evalStmt state' whileStmt
                in evalStmt state'' stmt
            False -> state'
        (Just val, _) -> throw $ IncorrectType (Id "while expression") "Bool" val
        (Nothing, _) -> throw CannotCombine

    FuncCallStmt call -> let (_, state') = evalFuncCall state call in state'

