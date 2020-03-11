{

{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Pascal.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToPosN
  )
where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z\_]

tokens :-
  $white+ ;

  -- value literals
  \' ([^\'] | "''")* \'      { tok_string TokenStr }
  \-? $digit+ \. $digit+     { tok_read TokenFloat }
  \-? $digit+                { tok_read TokenInt }

  -- comments
  -- (disallow nested comments)
  "//" .*                                             ; -- "//" comment
  "(*" ( \n | [^\*\(] | "("[^\*] | "*"[^\)] )* "*)"   ; -- "(* ... *)" comment
  "{" (\n | [^\{\}] )* "}"                            ; -- "{ ... }" comment

  -- operators
  [\+\-\*\/=]                                   { tok_string TokenOp }
  ">" | ">=" | "<" | "<=" | "==" | "<>"         { tok_string TokenOp }
  [\(\)] | ".." | ":=" | ":" | ";" | "," | "."  { tok_string TokenK }
  
  -- reserved words
  -- order does not matter -- Alex will pick the longest matching word
  "xor"|"while"|"var"|"true"|"to"|"then"|"string"|"program"|"procedure"|"or"|"of"|"not"|"mod"|"integer"|"if"|"function"|"for"|"false"|"end"|"else"|"downto"|"double"|"do"|"continue"|"const"|"case"|"break"|"boolean"|"begin"|"and" { tok_string TokenK }

  -- identifier
  $alpha [$alpha $digit]*   { tok_string TokenID }

{
data TokenClass
 = TokenInt Int
 | TokenFloat Float
 | TokenStr String
 | TokenID String
 | TokenOp String
 | TokenK String
 | TokenEOF
 deriving (Eq, Show)

data Token = Token AlexPosn TokenClass
  deriving (Eq, Show)

-- boilerplate
tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))
tok_read x = tok' (\s -> x (read (B.unpack s)))

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
