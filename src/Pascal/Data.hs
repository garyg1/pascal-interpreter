-- This file contains the data-structures for the AST
-- The role of the parser is to build the AST (Abstract Syntax Tree)

module Pascal.Data where

data Program = Program Id Block
    deriving (Show, Eq)

data Block = Block [Decl] [Stmt]
    deriving (Show, Eq)

data Decl = VarDecls [VarDecl]
    | ConstDecls [VarDecl]
    | FuncDecl FuncOrProc
    deriving (Show, Eq)

data VarDecl = Decl
    { name    :: Id
    , varType :: PascalType
    }
    | DeclTypeDefn
    { name    :: Id
    , varType :: PascalType
    , expr    :: Expr
    }
    | DeclDefn
    { name :: Id
    , expr :: Expr
    }
    deriving (Show, Eq)

data FuncOrProc = Func
    { fname       :: Id
    , params      :: [VarDecl]
    , returnType :: PascalType
    , block      :: Block
    }
    deriving (Show, Eq)

data Stmt = Stmts [Stmt]
    | IfStmt Expr Stmt
    | IfElseStmt Expr Stmt Stmt
    | CaseStmt Expr [CaseDecl]
    | CaseElseStmt Expr [CaseDecl] Stmt
    | WhileStmt Expr Stmt
    | ForToStmt Id Expr Expr Stmt
    | ForDownToStmt Id Expr Expr Stmt
    | Continue
    | Break
    | AssignStmt Id Expr
    | FuncCallStmt FuncCall
    deriving (Show, Eq)

data CaseDecl = CaseDecl [IntRange] Stmt
    deriving (Show, Eq)

data IntRange = IntRange Int Int
    deriving (Show, Eq)

data Expr = UnaryExpr String Expr
    | BinaryExpr String Expr Expr
    | FuncCallExpr FuncCall
    | VarExpr Id
    | IntExpr Int
    | StrExpr String
    | FltExpr Float
    | BoolExpr Bool
    deriving (Show, Eq)

newtype Id
    = Id {
        toString :: String
    }
    deriving (Eq, Ord)

instance Show Id where
    show id = "'Id: " ++ (toString id) ++ "'"

data FuncCall = FuncCall Id [Expr]
    deriving (Show, Eq)

data PascalType = TypeBool
    | TypeInt
    | TypeFloat
    | TypeString
    | TypeFunc
    | TypeNativeFunc
    | TypeNone -- unused: for procedure return types only
    deriving (Show, Eq)
