{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token

'+'             { Tplus }
'-'             { Tminus }   
'*'             { Ttimes }
'='             { Tequal }
'('             { Tlparen }
')'             { Trparen }
'&&'            { Tand }
'||'            { Tor }
'~'             { Tnot }
'=='            { Teq }
'>'             { Tgt }
'if'            { Tif }
'then'          { Tthen }
'else'          { Telse }
'error'         { Terror }
','             { Tcomma }
'.'             { Tdot }

ident           { Ident $$ }
num             { Number $$ }

%left '+' '-'
%left '*'
%left '&&' '||'
%left NOT
%nonassoc '==' '<='

%%

Prog :: { [Dec] }
    : Decs                                  { reverse $1 }

Decs :: { [Dec] }
    : Dec                                   { [$1] }
    | Decs Dec                              { $2 : $1 }

Dec :: { Dec }
    : ident '(' Params ')' '=' Exp '.'      { Dec $1 (length $3) ($6 $3) }

Params :: { [Ident] }
    : {- empty -}                           { [] }
    | Params1                               { reverse $1 }

Params1 :: { [Ident] }
    : ident                                 { [$1] }
    | Params ',' ident                      { $3 : $1 }

Exp :: { [Ident] -> Exp }
    : ident                                 { \p -> Var (params $1 p) }
    | num                                   { \_ -> Int $1 }
    | 'error'                               { \_ -> Error }
    | ident '(' Args ')'                    { \p -> Call $1 ($3 p) }
    | Exp '+' Exp                           { \p -> PrimOp AddOp [$1 p, $3 p] }
    | Exp '-' Exp                           { \p -> PrimOp SubOp [$1 p, $3 p] }
    | Exp '*' Exp                           { \p -> PrimOp MulOp [$1 p, $3 p] }
    | Exp '&&' Exp                          { \p -> PrimOp AndOp [$1 p, $3 p] }
    | Exp '||' Exp                          { \p -> PrimOp OrOp [$1 p, $3 p] }
    | '~' Exp   %prec NOT                   { \p -> PrimOp NotOp [$2 p] }
    | Exp '==' Exp                          { \p -> PrimOp EqOp [$1 p, $3 p] }
    | Exp '>' Exp                           { \p -> PrimOp GtOp [$1 p, $3 p] }
    | 'if' Exp 'then' Exp 'else' Exp        { \p -> Ite ($2 p) ($4 p) ($6 p) }
    | '(' Exp ')'                           { $2 }

Args :: { [Ident] -> [Exp] }
    : {- empty -}                           { \_ -> [] }
    | Args1                                 { \p -> reverse ($1 p) }

Args1 :: { [Ident] -> [Exp] }
    : Exp                                   { \p -> [$1 p] }
    | Args ',' Exp                          { \p -> $3 p : $1 p }

{
parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts

params :: Ident -> [Ident] -> Int
params y [] = error $ "Scope error: " ++ show y
params y (x : xs) = if x == y then 0 else params y xs + 1
}