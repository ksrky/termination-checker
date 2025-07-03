module Syntax
    ( Idx
    , Ident
    , PrimOp (..)
    , Exp (..)
    , Dec (..)
    , Params
    ) where

type Idx = Int
type Ident  = String

data PrimOp
    = AddOp
    | SubOp
    | MulOp
    | AndOp
    | OrOp
    | NotOp
    | EqOp
    | GtOp
    deriving (Eq, Show)

data Exp
    = Var Idx
    | Int Integer
    | Call Ident [Exp]
    | PrimOp PrimOp [Exp]
    | Ite Exp Exp Exp
    | Error
    deriving (Eq, Show)

data Dec = Dec
    { name  :: Ident
    , arity :: Int
    , value :: Exp
    }
    deriving (Eq, Show)

type Params = [Ident]
