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
    deriving (Show, Eq)

data InterpreterError
  = UnknownSymbol Id
  | NotImplemented
  deriving (Show)

instance Exception InterpreterError

eval :: Map.Map Id Value -> Expr -> Value
eval state expr = do
    case expr of
        VarExpr name -> case Map.lookup name state of
            Just val -> NamedValue name val
            Nothing -> throw $ UnknownSymbol name
        IntExpr i -> IntValue i
        StrExpr s -> StrValue s
        FltExpr f -> FloatValue f
        BoolExpr b -> BoolValue b
        BinaryExpr op b1 b2 -> combine op (eval state b1) (eval state b2)

-- strip name from NamedValue
combine :: String -> Value -> Value -> Value
combine op v1 v2 = case (v1, v2) of
    (NamedValue _ val1, v2) -> combine op val1 v2
    (v1, NamedValue _ val2) -> combine op v1 val2
    (IntValue i1, IntValue i2) -> IntValue $ i1 + i2
