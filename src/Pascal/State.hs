module Pascal.State where

import           Control.DeepSeq       (NFData)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)
import           Data.Functor
import           Data.Maybe            (fromJust)
import           GHC.Generics          (Generic)
import           Pascal.Data
import qualified Pascal.Scope          as Scope
import           System.IO.Streams     (InputStream, OutputStream)
import qualified System.IO.Streams     as Streams

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
type AppState = ExceptT Events (StateT PState (ReaderT AppIO IO))
type AppReturn a = (Either Events a, PState)
type AppIO = (InputStream ByteString, OutputStream ByteString)

runApp :: AppIO -> AppState a -> IO (AppReturn a)
runApp appIO fn = runReaderT (runStateT (runExceptT fn) new) appIO

new :: PState
new = PState [] Scope.empty

getline :: AppState String
getline = (ask >>= liftIO . Streams.read . fst) <&> (C.unpack . fromJust)

putline :: String -> AppState ()
putline line = ask >>= liftIO . Streams.write ((Just . C.pack) line) . snd

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
find name = gets $ find' name

find' :: Id -> PState -> Maybe Value
find' name = searchFor $ Scope.find name

isConst :: Id -> AppState (Maybe Bool)
isConst name = gets $ searchFor $ Scope.isConst name

searchFor :: (Scope.Scope Value -> Maybe a) -> PState -> Maybe a
searchFor fn = \case
    PState (sc : _) gl -> case fn sc of
        Just v  -> Just v
        Nothing -> fn gl
    PState [] gl -> fn gl

applyTo :: (Scope.Scope Value -> Scope.Scope Value) -> AppState ()
applyTo fn = state $ \case
    PState (sc : rest) gl -> ((), PState (fn sc : rest) gl)
    PState [] gl          -> ((), PState [] (fn gl))

findTop' :: Id -> PState -> Maybe Value
findTop' name = \case
    PState (sc : _) _ -> Scope.find name sc
    PState [] gl      -> Scope.find name gl

overwrite :: Id -> Value -> AppState ()
overwrite name value = applyTo (Scope.insert name value)

setConst :: Bool -> Id -> AppState ()
setConst isConst' name = applyTo (Scope.setConst isConst' name)

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
        Just _  -> Just $ PState (Scope.insert name val sc : rest) gl
        Nothing -> replace' name val (PState [] gl)
                    <&> \(PState _ gl') -> PState (sc : rest) gl'
    PState [] gl -> Scope.find name gl
                    <&> \_ ->  PState [] $ Scope.insert name val gl
