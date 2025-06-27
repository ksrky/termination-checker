module SMT.Syntax
    ( Aop (..)
    , AExp (..)
    , Bop (..)
    , BExp (..)
    , bnot
    ) where

data Aop = AAdd | ASub | AMul
    deriving (Show, Eq)

data AExp
    = AVar String
    | AInt Integer
    | Aop AExp Aop AExp
    deriving (Eq, Show)

data Bop = BEq | BNe | BGt | BGe | BLt | BLe
    deriving (Show, Eq)

data BExp
    = BVar String
    | BTrue
    | BFalse
    | BNot BExp
    | BAnd BExp BExp
    | BOr BExp BExp
    | Bop AExp Bop AExp
    deriving (Eq, Show)

bnot :: BExp -> BExp
bnot (BVar v)        = BNot (BVar v)
bnot BTrue           = BFalse
bnot BFalse          = BTrue
bnot (BNot e)        = e
bnot (BAnd e1 e2)    = BAnd (bnot e1) (bnot e2)
bnot (BOr e1 e2)     = BOr (bnot e1) (bnot e2)
bnot (Bop e1 BEq e2) = Bop e1 BNe e2
bnot (Bop e1 BNe e2) = Bop e1 BEq e2
bnot (Bop e1 BGt e2) = Bop e1 BLe e2
bnot (Bop e1 BGe e2) = Bop e1 BLt e2
bnot (Bop e1 BLt e2) = Bop e1 BGe e2
bnot (Bop e1 BLe e2) = Bop e1 BGt e2
