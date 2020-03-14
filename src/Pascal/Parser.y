{
module Pascal.Parser where

import Pascal.Base
import Pascal.Data
import Pascal.Lexer
}


%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }
%error {parseError}

%token
        strlit          { Token _ (TokenStr $$) }
        floatlit        { Token _ (TokenFloat $$) }
        intlit          { Token _ (TokenInt $$) }
        ID              { Token _ (TokenID $$) }

        '+'             { Token _ (TokenOp "+") }
        '-'             { Token _ (TokenOp "-") }
        '*'             { Token _ (TokenOp "*") }
        '/'             { Token _ (TokenOp "/") }
        '='             { Token _ (TokenOp "=") }
        
        '..'            { Token _ (TokenK "..") }
        ':='            { Token _ (TokenK ":=") }
        ')'             { Token _ (TokenK ")") }
        '('             { Token _ (TokenK "(") }
        ':'             { Token _ (TokenK ":") }
        ';'             { Token _ (TokenK ";") }
        ','             { Token _ (TokenK ",") }
        '.'             { Token _ (TokenK ".") }

        '>'             { Token _ (TokenOp ">")}
        '>='            { Token _ (TokenOp ">=")}
        '<'             { Token _ (TokenOp "<")}
        '<='            { Token _ (TokenOp "<=")}
        '=='            { Token _ (TokenOp "==")}
        '<>'            { Token _ (TokenOp "<>")}

        'xor'           { Token _ (TokenK "xor") }
        'while'         { Token _ (TokenK "while") }
        'var'           { Token _ (TokenK "var") }
        'true'          { Token _ (TokenK "true") }
        'to'            { Token _ (TokenK "to") }
        'then'          { Token _ (TokenK "then") }
        'string'        { Token _ (TokenK "string") }
        'program'       { Token _ (TokenK "program") }
        'procedure'     { Token _ (TokenK "procedure") }
        'or'            { Token _ (TokenK "or") }
        'of'            { Token _ (TokenK "of") }
        'not'           { Token _ (TokenK "not") }
        'mod'           { Token _ (TokenK "mod") }
        'integer'       { Token _ (TokenK "integer") }
        'if'            { Token _ (TokenK "if") }
        'function'      { Token _ (TokenK "function") }
        'for'           { Token _ (TokenK "for") }
        'false'         { Token _ (TokenK "false") }
        'end'           { Token _ (TokenK "end") }
        'else'          { Token _ (TokenK "else") }
        'downto'        { Token _ (TokenK "downto") }
        'double'        { Token _ (TokenK "double") }
        'do'            { Token _ (TokenK "do") }
        'continue'      { Token _ (TokenK "continue") }
        'const'         { Token _ (TokenK "const") }
        'case'          { Token _ (TokenK "case") }
        'break'         { Token _ (TokenK "break") }
        'boolean'       { Token _ (TokenK "boolean") }
        'begin'         { Token _ (TokenK "begin") }
        'and'           { Token _ (TokenK "and") }

%nonassoc '>' '>=' '<' '<=' '==' '!='
%left '+' '-'
%left '*' '/'
%nonassoc ':='
%%

-- Entry point
Program :: {Program}
    : ProgramDecl Block '.'     { Program $1 $2 }

ProgramDecl :: {Id}
    : 'program' ID ';'          { Id $2 }

Block :: {Block}
    : DeclSections CompoundStmt { Block $1 $2 }

DeclSections :: {[Decl]}
    : { [] }
    | DeclSection DeclSections  { $1 : $2 }

DeclSection :: {Decl}
    : 'var' VarDecls            { VarDecls $2 }
    | 'const' VarDecls          { ConstDecls $2 }
    | 'function' Func           { FuncDecl $2 }
    | 'procedure' Proc          { FuncDecl $2 }

-- used for both const and var
VarDecls :: {[VarDecl]}
    :                           { [] }
    | VarDecl ';' VarDecls      { $1 ++ $3 }

VarDecl :: {[VarDecl]}
    : DeclDefn                  { [$1] }
    | Decl                      { $1 }

-- declaration with inital value
DeclDefn :: {VarDecl}
    : ID ':' Type '=' Expr      { DeclTypeDefn (Id $1) $3 $5 }
    | ID '=' Expr               { DeclDefn (Id $1) $3 }

-- declaration with no initial value
Decl :: {[VarDecl]}
    : ID ':' Type               { [Decl (Id $1) $3] }
    | IdList ':' Type           { map (\x -> Decl x $3) $1 }

Type :: {PascalType}
    : 'double'                  { TypeFloat }
    | 'boolean'                 { TypeBool }
    | 'integer'                 { TypeInt }
    | 'string'                  { TypeString }

IdList :: {[Id]}
    : ID                        { [Id $1] }
    | ID ',' IdList             { Id $1 : $3 }

Func :: {FuncOrProc}
    : ID '(' ParamList ')' ':' Type ';' Block ';' { Func (Id $1) $3 $6 $8 }

Proc :: {FuncOrProc}
    : ID '(' ParamList ')' ';' Block ';' { Proc (Id $1) $3 $6 }

ParamList :: {[VarDecl]}
    : { [] }
    | Decl                      { $1 }
    | 'var' Decl                { $2 }
    | Decl ';' ParamList        { $1 ++ $3 }
    | 'var' Decl ';' ParamList  { $2 ++ $4 }

Stmt :: {Stmt}
    : CompoundStmt              { Stmts $1 }
    | IfStmt                    { $1 }
    | CaseStmt                  { $1 }
    | WhileStmt                 { $1 }
    | ForToStmt                 { $1 }
    | KeywordStmt               { $1 }
    | AssignStmt                { $1 }
    | FuncCall                  { FuncCallStmt $1 }
    | '(' Stmt ')'              { $2 }

CompoundStmt :: {[Stmt]}
    : 'begin' Stmts 'end'       { $2 }

Stmts :: {[Stmt]}
    : Stmt ';'                  { [$1] }
    | Stmt ';' Stmts            { $1 : $3 }

IfStmt :: {Stmt}
    : 'if' Expr 'then' Stmt                             { IfStmt $2 $4 }
    | 'if' Expr 'then' Stmt 'else' Stmt                 { IfElseStmt $2 $4 $6 }

CaseStmt :: {Stmt}
    : 'case' Expr 'of' CaseDecls 'end'                  { CaseStmt $2 $4 }
    | 'case' Expr 'of' CaseDecls 'else' Stmt ';' 'end'  { CaseElseStmt $2 $4 $6 }

CaseDecls :: {[CaseDecl]}
    : CaseDecl ';' { [$1] }
    | CaseDecl ';' CaseDecls { $1 : $3 }

CaseDecl :: {CaseDecl}
    : IntList ':' Stmt                                  { CaseDecl $1 $3 }

IntList :: {[IntRange]}
    : intlit                                            { [IntRange $1 $1] }
    | intlit '..' intlit                                { [IntRange $1 $3] }
    | intlit ',' IntList                                { IntRange $1 $1 : $3 }
    | intlit '..' intlit ',' IntList                    { IntRange $1 $3 : $5 }

WhileStmt :: {Stmt}
    : 'while' Expr 'do' Stmt                            { WhileStmt $2 $4 }

ForToStmt :: {Stmt}
    : 'for' ID ':=' Expr 'to' Expr 'do' Stmt            { ForToStmt (Id $2) $4 $6 $8 }
    | 'for' ID ':=' Expr 'downto' Expr 'do' Stmt        { ForDownToStmt (Id $2) $4 $6 $8 }

KeywordStmt :: {Stmt}
    : 'continue'        { Continue }
    | 'break'           { Break }

AssignStmt :: {Stmt}
    : ID ':=' Expr      { AssignStmt (Id $1) $3 }

FuncCall :: {FuncCall}
    : ID '(' Args ')'   { FuncCall (Id $1) $3 }

Args :: {[Expr]}
    : { [] }
    | Expr              { [$1] }
    | Expr ',' Args     { $1 : $3 }

Expr :: {Expr}
    : ID                { VarExpr $ Id $1 }
    | strlit            { StrExpr $ tail . init $ $1 }
    | floatlit          { FltExpr $1 }
    | 'true'            { BoolExpr True }
    | 'false'           { BoolExpr False }
    | intlit            { IntExpr $1 }
    | FuncCall          { FuncCallExpr $1 }
    | '-' Expr          { UnaryExpr "-" $2 }
    | '(' Expr ')'      { $2 }
    | 'not' Expr        { UnaryExpr "not" $2 }
    | Expr '/' Expr     { BinaryExpr "/" $1 $3 }
    | Expr '*' Expr     { BinaryExpr "*" $1 $3 }
    | Expr 'mod' Expr   { BinaryExpr "mod" $1 $3 }
    | Expr 'and' Expr   { BinaryExpr "and" $1 $3 }
    | Expr '-' Expr     { BinaryExpr "-" $1 $3 }
    | Expr '+' Expr     { BinaryExpr "+" $1 $3 }
    | Expr 'or' Expr    { BinaryExpr "or" $1 $3 }
    | Expr 'xor' Expr   { BinaryExpr "xor" $1 $3 }
    | Expr '<=' Expr    { BinaryExpr "<=" $1 $3 }
    | Expr '>=' Expr    { BinaryExpr ">=" $1 $3 }
    | Expr '<' Expr     { BinaryExpr "<" $1 $3 }
    | Expr '>' Expr     { BinaryExpr ">" $1 $3 }
    | Expr '=' Expr     { BinaryExpr "=" $1 $3 }
    | Expr '<>' Expr    { BinaryExpr "<>" $1 $3 }

{
parseError e = do
  error $ "Parse error: " ++ (show e)
}