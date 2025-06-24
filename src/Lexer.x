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
\<\=                        { \_ -> Tle }
\~                          { \_ -> Tnot }
\&\&                        { \_ -> Tand }
\|\|                        { \_ -> Tor }
if                          { \_ -> Tif }
then                        { \_ -> Tthen }
else                        { \_ -> Telse }
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
    | Tle
    | Tnot
    | Tand
    | Tor
    | Tif
    | Tthen
    | Telse
    | Tcomma
    | Tdot
    | Ident String
    | Number Int
    deriving (Eq, Show)
}