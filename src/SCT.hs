module SCT (checkTermination) where

import Data.Maybe
import SCGraph
import Data.Set qualified as S

composeSetWith :: Ord a => (a -> a -> Maybe a) -> S.Set a -> S.Set a -> S.Set a
composeSetWith f xs ys =
    S.fromList $ catMaybes [ f x y | x <- S.toList xs, y <- S.toList ys ]

composeArc :: Arc -> Arc -> Maybe Arc
composeArc (Arc from1 to1 typ1) (Arc from2 to2 typ2)
    | to1 == from2 = Just Arc
        { arcType = typ1 <> typ2
        , arcFrom = from1
        , arcTo   = to2
        }
    | otherwise = Nothing

composeGraph :: SCGraph -> SCGraph -> Maybe SCGraph
composeGraph (SCGraph src1 tgt1 arcs1) (SCGraph src2 tgt2 arcs2)
    | tgt1 == src2 = Just SCGraph
        { scSource = src1
        , scTarget = tgt2
        , scArcs   = composeSetWith composeArc arcs1 arcs2
        }
    | otherwise = Nothing

calcClosure :: SCGraphSet -> SCGraphSet
calcClosure scgs = go scgs scgs
  where
    go :: SCGraphSet -> SCGraphSet -> SCGraphSet
    go known worklist
        | S.null worklist
        = known
        | let fromLeft = composeSetWith composeGraph worklist known
        , let fromRight = composeSetWith composeGraph known worklist
        , let newGraphs = (fromLeft `S.union` fromRight) `S.difference` known 
        = go (known `S.union` newGraphs) newGraphs

isIdempotent :: SCGraph -> Bool
isIdempotent g = composeGraph g g == Just g

hasStrictIdArc :: SCGraph -> Bool
hasStrictIdArc (SCGraph f g arcs) = sigName f == sigName g && any isStrictIdentity (S.toList arcs)
  where
    isStrictIdentity (Arc i j Strict) = i == j
    isStrictIdentity _ = False

checkTermination :: SCGraphSet -> [Sig]
checkTermination scgs = foldr (\g@SCGraph{scSource} acc ->
    if scSource `notElem` acc && hasStrictIdArc g then scSource : acc else acc) [] cands
  where
    cands = S.filter isIdempotent $ calcClosure scgs
