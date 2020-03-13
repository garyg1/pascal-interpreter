module Pascal.Interpret where

import Pascal.Data
import qualified Data.Map as Map
import Control.Exception

interpret :: Program -> String
-- TODO: write the interpreter
interpret (Program name block) = "Not implemented"

data Value
    = IntValue Int
    | StrValue String
    | FloatValue Float
    | BoolValue Bool
    | NamedValue Id Value
    | FuncValue FuncOrProc
    deriving (Show, Eq)

data InterpreterError
    = UnknownSymbol Id
    | CannotCombine
    | IncorrectType Id String Value -- name expected actual
    | NotImplemented
    deriving (Show)

data State
    = State [Scope] Scope -- stack global
    deriving (Show, Eq)

type Scope = (Map.Map Id Value)


mustFind :: State -> Id -> Value
mustFind state id = case find state id of
    Just a -> a
    Nothing -> throw $ UnknownSymbol id

find :: State -> Id -> Maybe Value
find (State (scope : rest) global) name = case findScope scope name of
    Just x -> Just x
    Nothing -> find (State rest global) name
find (State [] global) name = findScope global name

findScope :: Scope -> Id -> Maybe Value
findScope map name = case Map.lookup name map of
    Just val -> Just val
    Nothing -> Nothing

instance Exception InterpreterError

eval :: State -> Expr -> Maybe Value
eval state expr = case expr of
    VarExpr name -> Just $ mustFind state name
    IntExpr i -> Just $ IntValue i
    StrExpr s -> Just $ StrValue s
    FltExpr f -> Just $ FloatValue f
    BoolExpr b -> Just $ BoolValue b
    BinaryExpr op b1 b2 -> Just $ combine op (eval state b1) (eval state b2)
    FuncCallExpr f -> evalFuncCall state f

combine :: String -> Maybe Value -> Maybe Value -> Value
combine _ Nothing _ = throw $ CannotCombine
combine _ _ Nothing = throw $ CannotCombine
combine op (Just v1) (Just v2) = combine' op v1 v2

combine' :: String -> Value -> Value -> Value
combine' op (NamedValue _ val1) v2 = combine' op val1 v2
combine' op v1 (NamedValue _ val2) = combine' op v1 val2
combine' op (IntValue i1) (IntValue i2) = IntValue $ i1 + i2

evalFuncCall :: State -> FuncCall -> Maybe Value
evalFuncCall state (FuncCall name args) = case mustFind state name of
    FuncValue fop -> evalFuncCall' state fop args
    val -> throw $ IncorrectType name "function" val

evalFuncCall' :: State -> FuncOrProc -> [Expr] -> Maybe Value
evalFuncCall' state (Func name params rType block) args = Nothing
evalFuncCall' state (Proc name params block) args = Nothing
