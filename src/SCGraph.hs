module SCGraph
    ( Sig (..)
    , ArcType (..)
    , Arc (..)
    , ArcSet
    , SCGraph (..)
    , SCGraphSet
    ) where

import Syntax
import Data.Set qualified as S

data Sig = Sig
    { sigName  :: String
    , sigArity :: Int
    }
    deriving (Eq, Ord, Show)

data ArcType = Strict | NonStrict
    deriving (Eq, Ord, Show)

instance Semigroup ArcType where
    Strict <> _ = Strict
    NonStrict <> typ = typ

data Arc = Arc
    { arcFrom :: Idx
    , arcTo   :: Idx
    , arcType :: ArcType
    }
    deriving (Eq, Ord, Show)

type ArcSet = S.Set Arc

data SCGraph = SCGraph
    { scSource :: Sig
    , scTarget :: Sig
    , scArcs   :: ArcSet
    }
    deriving (Eq, Ord, Show)

type SCGraphSet = S.Set SCGraph
