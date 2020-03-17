module Pascal.State where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Char            (toLower)
import           Pascal.Data
import qualified Pascal.Scope         as Scope

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
    | VariableExpected String
    | InternalError String
    | CannotRead PascalType
    deriving (Show)
instance Exception InterpreterError

data Events = Continue
    | Break
    deriving (Show)
instance Exception Events

data Value = IntValue
    { getInt :: Int
    }
    | StrValue String
    | FloatValue Float
    | BoolValue Bool
    | NamedValue Id Value
    | FuncValue
    { getFunc :: FuncOrProc
    }
    | NativeFuncValue
    { getNativeFunc :: NativeFunc
    }
    deriving (Eq)

instance Show Value where
    show (IntValue i)        = show i
    show (StrValue s)        = s
    show (FloatValue f)      = show f
    show (BoolValue b)       = map toLower $ show b
    show (NamedValue _ v)    = show v
    show (FuncValue _)       = "<function>"
    show (NativeFuncValue _) = "<native-function>"


data NativeFunc = NativeFunc ([Value] -> AppState (Maybe Value))

instance Show NativeFunc where
    show _ = "<Native Function>"

instance Eq NativeFunc where
    _ == _ = False


valueOf :: PascalType -> Value
valueOf TypeBool   = BoolValue False
valueOf TypeInt    = IntValue 0
valueOf TypeFloat  = FloatValue 0.0
valueOf TypeString = StrValue ""
valueOf _          = throw NotImplemented

typeOf :: Value -> PascalType
typeOf (BoolValue _)       = TypeBool
typeOf (IntValue _)        = TypeInt
typeOf (FloatValue _)      = TypeFloat
typeOf (StrValue _)        = TypeString
typeOf (NamedValue _ val)  = typeOf val
typeOf (FuncValue _)       = TypeFunc
typeOf (NativeFuncValue _) = TypeNativeFunc


data PState = PState
    { stack  :: [Scope.Scope Value]
    , global :: Scope.Scope Value
    }
    deriving (Show, Eq)

type AppState = ExceptT Events (StateT PState IO)

runApp :: AppState a0 -> IO (Either Events a0, PState)
runApp f = runStateT (runExceptT f) new

declare :: Bool -> Id -> Value -> AppState ()
declare isConst name value = do
    st <- get
    case findTop' st name of
        Just _  -> throw $ DuplicateDeclaration name
        Nothing -> do
            overwrite name value
            if isConst
                then setConst True name
                else return ()

mustFind :: Id -> AppState (Value)
mustFind name = state $ \pstate -> case find' pstate name of
    Just x  -> (x, pstate)
    Nothing -> throw $ UnknownSymbol name

find :: Id -> AppState (Maybe Value)
find name = do
    st <- get
    return $ find' st name

find' :: PState -> Id -> Maybe Value
find' st name = case st of
    PState (sc : _) gl -> case Scope.find sc name of
            Just v  -> Just v
            Nothing -> Scope.find gl name
    PState [] gl -> Scope.find gl name

findTop' :: PState -> Id -> Maybe Value
findTop' st name = case st of
    PState (sc : _) _ -> Scope.find sc name
    PState [] gl      -> Scope.find gl name

overwrite :: Id -> Value -> AppState ()
overwrite name value = state $ \pstate -> case pstate of
    PState (sc : rest) gl -> ((), PState ((Scope.insert name value sc) : rest) gl)
    PState [] gl          -> ((), PState [] (Scope.insert name value gl))

setConst :: Bool -> Id -> AppState ()
setConst isConst name = state $ \pstate -> case pstate of
    PState (sc : rest) gl -> ((), PState ((Scope.setConst isConst name sc) : rest) gl)
    PState [] gl          -> ((), PState [] (Scope.setConst isConst name gl))

push :: Scope.Scope Value -> AppState ()
push scope = state $ \(PState scopes gl) -> ((), PState (scope : scopes) gl)

pushEmpty :: AppState ()
pushEmpty = push Scope.empty

pop :: AppState ()
pop = state $ \(PState (_ : rest) gl) -> (() , PState rest gl)

mustReplace :: Id -> Value -> AppState ()
mustReplace name value = state $ \pstate -> case replace' name value pstate of
    Just st -> ((), st)
    Nothing -> throw $ UnknownSymbol name

replace' :: Id -> Value -> PState -> Maybe PState
replace' name val state' = case state' of
    PState (sc : rest) gl -> case Scope.find sc name of
        Just _  -> let sc' = (Scope.insert name val sc) in Just $ PState (sc' : rest) gl
        Nothing -> case replace' name val $ PState [] gl of
            Just (PState _ gl') -> Just $ PState (sc : rest) gl'
            Nothing             -> Nothing
    PState [] gl -> case Scope.find gl name of
        Just _  -> let gl' = Scope.insert name val gl in Just $ PState [] gl'
        Nothing -> Nothing

new :: PState
new = PState [] Scope.empty
