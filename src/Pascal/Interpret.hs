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

instance Exception InterpreterError

eval :: Map.Map Id Value -> Expr -> Maybe Value
eval state expr = case expr of
    VarExpr name -> case Map.lookup name state of
        Just val -> Just $ NamedValue name val
        Nothing -> throw $ UnknownSymbol name
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

evalFuncCall :: Map.Map Id Value -> FuncCall -> Maybe Value
evalFuncCall state (FuncCall name args) = case Map.lookup name state of
    Just (FuncValue fop) -> evalFuncCall' state fop args
    Just val -> throw $ IncorrectType name "function" val
    Nothing -> throw $ UnknownSymbol name

evalFuncCall' :: Map.Map Id Value -> FuncOrProc -> [Expr] -> Maybe Value
evalFuncCall' state fop args = case fop of
    Func name params rType block -> Nothing
    Proc name params block -> Nothing
