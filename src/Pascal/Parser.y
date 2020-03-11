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

%token
        strlit          { Token _ (TokenStr $$) }
        floatlit        { Token _ (TokenFloat $$) }
        int             { Token _ (TokenInt $$) }
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
    : Statements { $1 }