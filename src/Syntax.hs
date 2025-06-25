module Syntax
    ( Ident
    , Exp (..)
    , Dec (..)
    , Params
    ) where

type Ident  = String

data Exp
    = Var Int
    | Int Int
    | Call Ident [Exp]
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | And Exp Exp
    | Or Exp Exp
    | Not Exp
    | Eq Exp Exp
    | Gt Exp Exp
    | Ite Exp Exp Exp
    deriving (Eq, Show)

data Dec
    = Dec Ident Exp
    deriving (Eq, Show)

type Params = [Ident]
