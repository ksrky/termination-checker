module Graph (mkSCGraph) where

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
    , ascArcs  :: [Arc]
    }

mkArc :: Exp -> Arc
mkArc _ = undefined

mkSCGraph :: Sig -> Exp -> [SCGraph]
mkSCGraph _ (Var _) = []
mkSCGraph _ (Int _) = []
mkSCGraph sig (Call fn args) =
    SCGraph{scSource = sig, scTarget = Sig{sigName = fn, sigArity = length args}, ascArcs = map mkArc args} :
        concatMap (mkSCGraph sig) args
mkSCGraph sig (PrimOp _ args) = concatMap (mkSCGraph sig) args
mkSCGraph sig (Ite e1 e2 e3) = mkSCGraph sig e1 ++ mkSCGraph sig e2 ++ mkSCGraph sig e3
