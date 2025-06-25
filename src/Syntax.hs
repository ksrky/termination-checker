module Syntax
    ( Idx
    , Ident
    , Exp (..)
    , Dec (..)
    , Params
    ) where

type Idx = Int
type Ident  = String

data Exp
    = Var Idx
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

data Dec = Dec
    { name  :: Ident
    , arity :: Int
    , value :: Exp
    }
    deriving (Eq, Show)

type Params = [Ident]
