module Graph
    ( Sig (..)
    , ArcType (..)
    , Arc (..)
    , SCGraph (..)
    ) where

import           Syntax

data Sig = Sig
    { sigName  :: String
    , sigArity :: Int
    }
    deriving (Eq, Show)

data ArcType = Strict | NonStrict
    deriving (Eq, Show)

data Arc = Arc
    { arcType :: ArcType
    , arcFrom :: Idx
    , arcTo   :: Idx
    }
    deriving (Eq, Show)

data SCGraph = SCGraph
    { scSource :: Sig
    , scTarget :: Sig
    , scArcs   :: [Arc]
    }
