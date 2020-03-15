module Pascal.State where

import           Control.Exception
import           Control.Monad.State
import qualified Data.Map            as Map
import           Pascal.Data

data InterpreterError = UnknownSymbol Id
    | CannotCombine
    | CannotEval
    | IncorrectType
    { location  :: String
    , expected :: PascalType
    , actual   :: Value
    }
    | IncorrectArgs Id [Value] [VarDecl]
    | NotImplemented
    | DuplicateDeclaration Id
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

valueOf :: PascalType -> Value
valueOf TypeBool   = BoolValue False
valueOf TypeInt    = IntValue 0
valueOf TypeFloat  = FloatValue 0.0
valueOf TypeString = StrValue ""
valueOf _          = throw NotImplemented

data PState = PState
    { stack  :: [Scope]
    , global :: Scope
    }
    deriving (Show, Eq)

type AppState = StateT PState IO

runApp :: StateT PState m0 a0 -> m0 (a0, PState)
runApp f = runStateT f new

type Scope = (Map.Map Id Value)

declare :: Id -> Value -> AppState ()
declare name value = do
    case return (findInTopScope name) of
        Just _  -> overwrite name value
        Nothing -> throw $ DuplicateDeclaration name

mustFind :: Id -> AppState Value
mustFind name = state $ \pstate -> case find' pstate name of
    Just x  -> (x, pstate)
    Nothing -> throw $ UnknownSymbol name

findInTopScope :: Id -> AppState (Maybe Value)
findInTopScope name = do
    st <- get
    return $ case st of
        (PState (sc : _) _) -> findScope sc name
        (PState [] gl)      -> findScope gl name

find :: Id -> AppState (Maybe Value)
find name = state $ \pstate -> case find' pstate name of
    Just x  -> (Just x, pstate)
    Nothing -> (Nothing, pstate)

find' :: PState -> Id -> Maybe Value
find' (PState (scope : rest) global) name = case findScope scope name of
        Just x  -> Just x
        Nothing -> find' (PState rest global) name
find' (PState [] global) name = findScope global name

findScope :: Scope -> Id -> Maybe Value
findScope map name = case Map.lookup name map of
    Just val -> Just val
    Nothing  -> Nothing

overwrite :: Id -> Value -> AppState ()
overwrite name value = state $ \pstate -> case pstate of
    PState (scope : rest) global -> ((), PState ((Map.insert name value scope) : rest) global)
    PState [] global             -> ((), PState [] (Map.insert name value global))

push :: Scope -> AppState ()
push scope = state $ \(PState scopes global) -> ((), PState (scope : scopes) global)

pushEmpty :: AppState ()
pushEmpty = do
    push Map.empty
    return ()

mustReplace :: Id -> Value -> AppState ()
mustReplace name value = state $ \pstate -> case mustReplace' name value pstate of
    Just st -> ((), st)
    Nothing -> throw $ UnknownSymbol name

mustReplace' :: Id -> Value -> PState -> Maybe PState
mustReplace' name val state = case state of
    (PState (sc : rest) global) -> case findScope sc name of
        Just x  -> let
            sc' = (Map.insert name val sc)
            in Just $ PState (sc' : rest) global
        Nothing -> case mustReplace' name val $ PState rest global of
            Just (PState rest' global') -> Just $ PState (sc : rest') global
            Nothing                     -> Nothing
    (PState [] global) -> case findScope global name of
        Just x  -> let
            global' = Map.insert name val global
            in Just $ PState [] global'
        Nothing -> Nothing

new :: PState
new = PState [] Map.empty
