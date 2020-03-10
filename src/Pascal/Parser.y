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
        keyword         { Token _ (TokenK $$) }
%%

-- Entry point
Program :: {Program}
    : Statements { $1 }

Statements :: {[String]}
    : { [] } -- nothing; make empty list
    | Statement Statements { $1:$2 } -- put statement as first element of statements

Statement :: {String}
    : int { "int" }
    | ID { $1 }
    | strlit { $1 }
    | floatlit { "float" }
    | keyword { "keyword" ++ $1 }