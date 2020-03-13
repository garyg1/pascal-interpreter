module Pascal.State where

import           Control.Exception
import qualified Data.Map          as Map
import           Pascal.Data

data InterpreterError = UnknownSymbol Id
    | CannotCombine
    | CannotEval
    | IncorrectType Id String Value
    | IncorrectArgs Id [Value] [VarDecl]
    | NotImplemented
    | InternalError String
    deriving (Show)
instance Exception InterpreterError

data Value = IntValue Int
    | StrValue String
    | FloatValue Float
    | BoolValue Bool
    | NamedValue Id Value
    | FuncValue FuncOrProc
    deriving (Show, Eq)

data State = State
    { stack  :: [Scope]
    , global :: Scope
    }
    deriving (Show, Eq)

type Scope = (Map.Map Id Value)

mustFind :: State -> Id -> Value
mustFind state name = case find state name of
    Just a  -> a
    Nothing -> throw $ UnknownSymbol name

find :: State -> Id -> Maybe Value
find (State (scope : rest) global) name = case findScope scope name of
    Just x  -> Just x
    Nothing -> find (State rest global) name
find (State [] global) name = findScope global name

findScope :: Scope -> Id -> Maybe Value
findScope map name = case Map.lookup name map of
    Just val -> Just val
    Nothing  -> Nothing

put :: State -> Id -> Value -> State
put state name value = case state of
    State (scope : rest) global -> State ((Map.insert name value scope) : rest) global
    State [] global -> State [] (Map.insert name value global)

push :: Scope -> State -> State
push scope (State scopes global) = State (scope : scopes) global

replace :: Id -> Value -> State -> State
replace name value (State (scope : rest) global) = case findScope scope name of
    Just x  -> State ((Map.insert name value scope) : rest) global
    Nothing -> push scope $ replace name value $ State rest global
replace name value (State [] global) = State [] $ Map.insert name value global
