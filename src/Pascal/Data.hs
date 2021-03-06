-- This file contains the data-structures for the AST
-- The role of the parser is to build the AST (Abstract Syntax Tree)

module Pascal.Data where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

data Program = Program Id Block
    deriving (Show, Eq, NFData, Generic)

data Block = Block
    { blockDecls :: [Decl]
    , blockStmts :: [Stmt]
    }
    deriving (Show, Eq, NFData, Generic)

data Decl = VarDecls [VarDecl]
    | ConstDecls [VarDecl]
    | FuncDecl Func
    deriving (Show, Eq, NFData, Generic)

data VarDecl = Decl
    { dname   :: Id
    , varType :: PascalType
    }
    | DeclTypeDefn
    { dname   :: Id
    , varType :: PascalType
    , expr    :: Expr
    }
    | DeclDefn
    { dname :: Id
    , expr  :: Expr
    }
    deriving (Show, Eq, NFData, Generic)

data Func = Func
    { fname      :: Id
    , params     :: [VarDecl]
    , returnType :: PascalType
    , block      :: Block
    }
    deriving (Show, Eq, NFData, Generic)

data Stmt = AssignStmt Id Expr
    | BreakStmt
    | CaseElseStmt Expr [CaseDecl] Stmt
    | CaseStmt Expr [CaseDecl]
    | ContinueStmt
    | ForDownToStmt Id Expr Expr Stmt
    | ForToStmt Id Expr Expr Stmt
    | FuncCallStmt FuncCall
    | IfElseStmt Expr Stmt Stmt
    | IfStmt Expr Stmt
    | Stmts [Stmt]
    | WhileStmt
    { getWhileExpr :: Expr
    , getWhileStmt :: Stmt
    }
    deriving (Show, Eq, NFData, Generic)

data CaseDecl = CaseDecl [IntRange] Stmt
    deriving (Show, Eq, NFData, Generic)

data IntRange = IntRange Int Int
    deriving (Show, Eq, NFData, Generic)

data Expr = UnaryExpr String Expr
    | BinaryExpr String Expr Expr
    | FuncCallExpr FuncCall
    | VarExpr Id
    | IntExpr Int
    | StrExpr String
    | FltExpr Float
    | BoolExpr Bool
    deriving (Show, Eq, NFData, Generic)

newtype Id
    = Id {
        toString :: String
    }
    deriving (Eq, Ord, NFData, Generic)

instance Show Id where
    show _id = "(Id " ++ toString _id ++ ")"

data FuncCall = FuncCall Id [Expr]
    deriving (Show, Eq, NFData, Generic)

data PascalType = TypeBool
    | TypeInt
    | TypeFloat
    | TypeString
    | TypeFunc
    | TypeNativeFunc
    | TypeNone
    deriving (Show, Eq, NFData, Generic)
