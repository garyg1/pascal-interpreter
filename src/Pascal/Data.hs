-- This file contains the data-structures for the AST
-- The role of the parser is to build the AST (Abstract Syntax Tree) 

module Pascal.Data
    (
        Program
    ) where

data Program = Program Block

data Block = Block [Decl] CompoundStmt

data Decl
    = VarDecls [VarDecl]
    | ConstDecls [VarDecl]
    | FuncDecl FuncOrProc

data VarDecl
    = Decls [Id] PascalType
    | Decl Id PascalType
    | DeclTypeDefn Id PascalType Expr
    | DeclDefn Id Expr

data FuncOrProc
    = Func Id [Parameter] PascalType [Decl] CompoundStmt
    | Proc Id [Parameter] [Decl] CompoundStmt

data PascalType = TypeBool | TypeInt | TypeFloat | TypeString

data Stmt
  = Stmts CompoundStmt
  | IfStmt IfStmt
  | CaseStmt CaseStmt
  | WhileStmt WhileStmt
  | ForToStmt ForToStmt
  | KeywordStmt KeywordStmt
  | AssignStmt AssignStmt
  | FuncCall FuncCall

type CompoundStmt = [Stmt]

data IfStmt
    = IfStmt Expr Stmt
    | IfElseStmt Expr Stmt Stmt

data CaseStmt
    = CaseStmt Expr Cases
    | CaseElseStmt Expr Cases Stmt

type CaseDecls = [CaseDecl]
data CaseDecl = [IntOrRange] Stmt

data IntOrRange = Integer Integer | IntRange IntRange
data IntRange = IntRange Integer Integer

data WhileStmt = WhileStmt Expr Stmt

data ForToStmt = ForToStmt Var Expr ToKeyword Expr Stmt
data ToKeyword = To | DownTo
data ForKeyword = Continue | Break

data AssignStmt = AssignStmt Id Expr

data Expr
    = UnaryExpr String Expr
    | BinaryExpr String Expr Expr
    | FuncCall Var ArgsList
    | Var String
    | Integer Integer
    | String String

data ArgsList = [Expr]