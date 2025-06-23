module Term
    ( Var (..)
    , Term (..)
    , Type (..)
    , Command (..)
    ) where

data Idx = Z | S Idx
    deriving (Eq, Show)

type Name = String

data Var
    = Bvar Idx 
    | Fvar Name
    deriving (Eq, Show)

type Elim = Term

data Term
    = Ne Var [Elim]
    deriving (Eq, Show)

data Type
    = Type
    | Arrow Type Type
    deriving (Eq, Show)

data Command
    = Decl Name Type
    | Eqn Term Term
    deriving (Eq, Show)
