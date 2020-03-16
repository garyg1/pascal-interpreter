module Pascal.State where

import           Control.Exception
import           Control.Monad.State
import qualified Data.Map            as Map
import           Pascal.Data

data InterpreterError = UnknownSymbol Id
    | CannotCombine
    | CannotEval
    | IncorrectType
    { location :: String
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
    | FuncValue
    { getFunc :: FuncOrProc
    }
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
    st <- get
    case findTop' st name of
        Just _  -> throw $ DuplicateDeclaration name
        Nothing -> overwrite name value

mustFind :: Id -> AppState Value
mustFind name = state $ \pstate -> case find' pstate name of
    Just x  -> (x, pstate)
    Nothing -> throw $ UnknownSymbol name

find :: Id -> AppState (Maybe Value)
find name = do
    st <- get
    return $ find' st name

find' :: PState -> Id -> Maybe Value
find' st name = case st of
    PState (sc : _) gl -> case findScope sc name of
            Just v  -> Just v
            Nothing -> findScope gl name
    PState [] gl -> findScope gl name

findTop' :: PState -> Id -> Maybe Value
findTop' st name = case st of
    PState (sc : _) _ -> findScope sc name
    PState [] gl      -> findScope gl name

findScope :: Scope -> Id -> Maybe Value
findScope map name = case Map.lookup name map of
    Just val -> Just val
    Nothing  -> Nothing

overwrite :: Id -> Value -> AppState ()
overwrite name value = state $ \pstate -> case pstate of
    PState (sc : rest) global -> ((), PState ((Map.insert name value sc) : rest) global)
    PState [] global          -> ((), PState [] (Map.insert name value global))

push :: Scope -> AppState ()
push scope = state $ \(PState scopes global) -> ((), PState (scope : scopes) global)

pushEmpty :: AppState ()
pushEmpty = push Map.empty

pop :: AppState ()
pop = state $ \(PState (_ : rest) global) -> (() , PState rest global)

mustReplace :: Id -> Value -> AppState ()
mustReplace name value = state $ \pstate -> case replace' name value pstate of
    Just st -> ((), st)
    Nothing -> throw $ UnknownSymbol name

replace' :: Id -> Value -> PState -> Maybe PState
replace' name val state = case state of
    PState (sc : rest) gl -> case findScope sc name of
        Just _  -> let sc' = (Map.insert name val sc) in Just $ PState (sc' : rest) gl
        Nothing -> case replace' name val $ PState [] gl of
            Just (PState _ gl') -> Just $ PState (sc : rest) gl'
            Nothing             -> Nothing
    PState [] gl -> case findScope gl name of
        Just x  -> let gl' = Map.insert name val gl in Just $ PState [] gl'
        Nothing -> Nothing
new :: PState
new = PState [] Map.empty
