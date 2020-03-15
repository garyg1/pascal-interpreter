module Pascal.State where

import           Control.Exception
import qualified Data.Map                 as Map
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

data PState = PState
    { stack  :: [Scope]
    , global :: Scope
    }
    deriving (Show, Eq)

type Scope = (Map.Map Id Value)

mustFind :: PState -> Id -> Value
mustFind state name = case find state name of
    Just a  -> a
    Nothing -> throw $ UnknownSymbol name

find :: PState -> Id -> Maybe Value
find (PState (scope : rest) global) name = case findScope scope name of
    Just x  -> Just x
    Nothing -> find (PState rest global) name
find (PState [] global) name = findScope global name

findScope :: Scope -> Id -> Maybe Value
findScope map name = case Map.lookup name map of
    Just val -> Just val
    Nothing  -> Nothing

overwrite :: PState -> Id -> Value -> PState
overwrite state name value = case state of
    PState (scope : rest) global -> PState ((Map.insert name value scope) : rest) global
    PState [] global -> PState [] (Map.insert name value global)

push :: Scope -> PState -> PState
push scope (PState scopes global) = PState (scope : scopes) global

replace :: Id -> Value -> PState -> PState
replace name value (PState (scope : rest) global) = case findScope scope name of
    Just x  -> PState ((Map.insert name value scope) : rest) global
    Nothing -> push scope $ replace name value $ PState rest global
replace name value (PState [] global) = PState [] $ Map.insert name value global

new :: PState
new = PState [] Map.empty
