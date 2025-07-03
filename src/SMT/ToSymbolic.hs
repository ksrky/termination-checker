module SMT.ToSymbolic
    ( proveWithAssump
    ) where

import           Control.Monad.State
import           Data.SBV
import           SMT.Syntax

type SymVars = [(String, SInteger)]

type SymM = StateT SymVars Symbolic

runSymM :: SymVars -> SymM a -> Symbolic (a, SymVars)
runSymM vars action = runStateT action vars

class ToSymbolic a b where
    toSymbolic :: a -> SymM (SBV b)

instance ToSymbolic AExp Integer where
    toSymbolic (AVar x) = do
        vars <- get
        case lookup x vars of
            Just v  -> return v
            Nothing -> do
                v <- lift $ sInteger x
                modify ((x, v) :)
                return v
    toSymbolic (AInt n) = return $ literal n
    toSymbolic (Aop e1 aop e2) = do
        v1 <- toSymbolic e1
        v2 <- toSymbolic e2
        return $ case aop of
            AAdd -> v1 + v2
            ASub -> v1 - v2
            AMul -> v1 * v2

instance ToSymbolic BExp Bool where
    toSymbolic (BVar x) = do
        vars <- get
        case lookup x vars of
            Just v  -> return $ v ./= 0
            Nothing -> lift $ sBool x
    toSymbolic BTrue = return sTrue
    toSymbolic BFalse = return sFalse
    toSymbolic (BNot e) = sNot <$> toSymbolic e
    toSymbolic (BAnd e1 e2) = (.&&) <$> toSymbolic e1 <*> toSymbolic e2
    toSymbolic (BOr e1 e2) = (.||) <$> toSymbolic e1 <*> toSymbolic e2
    toSymbolic (Bop e1 bop e2) = do
        v1 :: SInteger <- toSymbolic e1
        v2 :: SInteger <- toSymbolic e2
        return $ case bop of
            BEq -> v1 .== v2
            BNe -> v1 ./= v2
            BGt -> v1 .> v2
            BGe -> v1 .>= v2
            BLt -> v1 .< v2
            BLe -> v1 .<= v2

proveWithAssump :: [BExp] -> BExp -> IO Bool
proveWithAssump assumptions goal = do
    isSat <- isSatisfiable $ do
        (constrs, vars) <- runSymM [] $ mapM toSymbolic assumptions
        (sgoal, _) <- runSymM vars $ toSymbolic (bnot goal)
        return $ sAnd (sgoal : constrs)
    return $ not isSat
