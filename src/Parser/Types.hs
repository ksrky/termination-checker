{-# LANGUAGE DeriveLift #-}

{-|
Common types used in the P3 parser.
-}
module Parser.Types
    ( Name (..)
    , BindingPower (..)
    , T (..)
    , Token (..)
    , Oper (..)
    , MixfixOp
    , Syntax (..)
    , SyntaxStack
    ) where

-- | Parser name, which can be a node label of the syntax tree.
newtype Name = Name String
    deriving (Eq, Show)

-- | Each operand has a binding power.
-- min = 0, max = 100
newtype BindingPower = BindingPower Int
    deriving (Eq, Ord, Show, Enum)

instance Bounded BindingPower where
    minBound = BindingPower 0
    maxBound = BindingPower 100

data T
    = Letter String
    | Symbol String
    deriving (Eq, Ord, Show)

data Token
    = Token T
    | Terminator
    deriving (Eq, Ord, Show)


-- | Operator or Operand
data Oper
    = Operator T
    | Operand BindingPower
    deriving (Eq, Show)

type MixfixOp = [Oper]

data Syntax
    = App MixfixOp [Syntax]
    | Var T
    deriving (Eq, Show)

type SyntaxStack = [Syntax]
