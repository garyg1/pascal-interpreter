module Pascal.State where

import           Control.DeepSeq      (NFData)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Char            (toLower)
import           GHC.Generics         (Generic)
import           Pascal.Data
import qualified Pascal.Scope         as Scope

data InterpreterError = UnknownSymbol Id
    | IncorrectType
    { location :: String
    , expected :: PascalType
    , actual   :: Value
    }
    | IncorrectArgs Id [Value] [VarDecl]
    | NotImplemented
    | DuplicateDeclaration Id
    | CannotBeConst Id
    | VariableExpected String
    | UndeclaredSymbol Id
    | InternalError String
    | CannotCast PascalType Value
    | CannotCombine String String
    | CannotEval Expr
    | CannotRead PascalType
    deriving (Show, Eq, NFData, Generic)
instance Exception InterpreterError

data Events = Continue
    | Break
    deriving (Show, Eq, NFData, Generic)
instance Exception Events

data Value = IntValue
    { getInt :: Int
    }
    | StrValue String
    | FloatValue Float
    | BoolValue Bool
    | NamedValue
    { getName  :: Id
    , getValue :: Value
    }
    | FuncValue
    { getFunc :: Func
    }
    | NativeFuncValue
    { getNativeFunc :: NativeFunc
    }
    deriving (Eq, NFData, Generic)

instance Show Value where
    show (IntValue i)        = show i
    show (StrValue s)        = s
    show (FloatValue f)      = show f
    show (BoolValue b)       = map toLower $ show b
    show (NamedValue _ v)    = show v
    show (FuncValue _)       = "<function>"
    show (NativeFuncValue _) = "<native-function>"

debugShow :: Value -> String
debugShow (NamedValue n v) = "NamedValue '" ++ toString n ++ "'" ++ show v
debugShow other            = show other

data NativeFunc = NativeFunc
    { nfName :: Id
    , nfFunc :: [Value] -> AppState (Maybe Value)
    }
    deriving (NFData, Generic)

instance Show NativeFunc where
    show _ = "<Native Function>"

instance Eq NativeFunc where
    (NativeFunc n1 _) == (NativeFunc n2 _) = n1 == n2

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
    deriving (Show, Eq, NFData, Generic)

{- we use exceptions for "continue" and "break",
so State must be returned EVEN IF there's an Monad.Except event -}
type AppState = ExceptT Events (StateT PState IO)
type AppReturn a = (Either Events a, PState)

runApp :: AppState a -> IO (AppReturn a)
runApp f = runStateT (runExceptT f) new

new :: PState
new = PState [] Scope.empty

declare :: Bool -> Id -> Value -> AppState ()
declare isConst' name value = do
    st <- get
    case findTop' name st of
        Just _  -> throw $ DuplicateDeclaration name
        Nothing -> do
            overwrite name value
            when isConst' $
                setConst True name

declareVar :: Id -> Value -> AppState ()
declareVar = declare False

declareConst :: Id -> Value -> AppState ()
declareConst = declare True

mustFind :: Id -> AppState Value
mustFind name = state $ \pstate -> case find' name pstate of
    Just x  -> (x, pstate)
    Nothing -> throw $ UnknownSymbol name

find :: Id -> AppState (Maybe Value)
find name = get >>= (pure . find' name)

find' :: Id -> PState -> Maybe Value
find' name = \case
    PState (sc : _) gl -> case Scope.find name sc of
        Just v  -> Just v
        Nothing -> Scope.find name gl
    PState [] gl -> Scope.find name gl

findTop' :: Id -> PState -> Maybe Value
findTop' name = \case
    PState (sc : _) _ -> Scope.find name sc
    PState [] gl      -> Scope.find name gl

overwrite :: Id -> Value -> AppState ()
overwrite name value = applyToTopScope (Scope.insert name value)

setConst :: Bool -> Id -> AppState ()
setConst isConst' name = applyToTopScope (Scope.setConst isConst' name)

applyToTopScope :: (Scope.Scope Value -> Scope.Scope Value) -> AppState ()
applyToTopScope fn = state $ \case
    PState (sc : rest) gl -> ((), PState (fn sc : rest) gl)
    PState [] gl          -> ((), PState [] (fn gl))

isConst :: Id -> AppState (Maybe Bool)
isConst name = do
    st <- get
    return $ case st of
        PState [] gl       -> Scope.isConst name gl
        PState (sc : _) gl -> case Scope.isConst name sc of
            Just v  -> Just v
            Nothing -> Scope.isConst name gl

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
    PState (sc : rest) gl -> case Scope.find name sc of
        Just _  -> let sc' = Scope.insert name val sc in Just $ PState (sc' : rest) gl
        Nothing -> case replace' name val $ PState [] gl of
            Just (PState _ gl') -> Just $ PState (sc : rest) gl'
            Nothing             -> Nothing
    PState [] gl -> case Scope.find name gl of
        Just _  -> let gl' = Scope.insert name val gl in Just $ PState [] gl'
        Nothing -> Nothing

