{
module Lexer
    ( Token (..)
    , alexScanTokens
    ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@decimal = $digit+
@ident = $alpha [$alpha $digit \_ \']*

tokens :-

$white+                     ;
"--".*                      ;
\+                          { \_ -> Tplus }
\-                          { \_ -> Tminus }
\*                          { \_ -> Ttimes }
\=                          { \_ -> Tequal }
\(                          { \_ -> Tlparen }
\)                          { \_ -> Trparen }
\=\=                        { \_ -> Teq }
\>                          { \_ -> Tgt }
\~                          { \_ -> Tnot }
\&\&                        { \_ -> Tand }
\|\|                        { \_ -> Tor }
if                          { \_ -> Tif }
then                        { \_ -> Tthen }
else                        { \_ -> Telse }
error                       { \_ -> Terror }
\,                          { \_ -> Tcomma }
\.                          { \_ -> Tdot }
@ident                      { Ident }
@decimal                    { Number . read }

{
data Token
    = Tplus
    | Tminus
    | Ttimes
    | Tequal
    | Tlparen
    | Trparen
    | Teq
    | Tgt
    | Tnot
    | Tand
    | Tor
    | Tif
    | Tthen
    | Telse
    | Terror
    | Tcomma
    | Tdot
    | Ident String
    | Number Integer
    deriving (Eq, Show)
}