{-# LANGUAGE ImpredicativeTypes #-}

module FlowAnalysis (mkSCGraphs) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Set             qualified as S
import SCGraph
import SMT.Syntax           qualified as SMT
import SMT.ToSymbolic
import Syntax

data SCContext = SCContext
    { scCallee     :: Sig
    , scConditions :: [SMT.BExp]
    }

type SCM = ReaderT SCContext (StateT SCGraphSet IO)

runSCM :: SCContext -> SCM a -> IO a
runSCM ctx action = evalStateT (runReaderT action ctx) S.empty

addGraph :: SCGraph -> SCM ()
addGraph = modify . S.insert

withCondition :: SMT.BExp -> SCM a -> SCM a
withCondition c = local $ \s -> s { scConditions = c : scConditions s }

toAVar :: Idx -> SMT.AExp
toAVar i = SMT.AVar ("#" ++ show i)

toAExp :: Exp -> Maybe SMT.AExp
toAExp (Var x) = Just $ toAVar x
toAExp (Int n) = Just $ SMT.AInt n
toAExp (Call _ _) = Nothing
toAExp (PrimOp op [e1, e2]) = do
    a1 <- toAExp e1
    a2 <- toAExp e2
    case op of
        AddOp -> Just $ SMT.Aop a1 SMT.AAdd a2
        SubOp -> Just $ SMT.Aop a1 SMT.ASub a2
        MulOp -> Just $ SMT.Aop a1 SMT.AMul a2
        _     -> Nothing
toAExp _ = Nothing

toBExp :: Exp -> Maybe SMT.BExp
toBExp (Var x) = Just $ SMT.BVar ("x" ++ show x)
toBExp (Int n)
    | n == 0 = Just SMT.BFalse
    | otherwise = Just SMT.BTrue
toBExp (Call _ _) = Nothing
toBExp (PrimOp op [e1, e2]) = do
    a1 <- toAExp e1
    a2 <- toAExp e2
    case op of
        EqOp -> Just $ SMT.Bop a1 SMT.BEq a2
        GtOp -> Just $ SMT.Bop a1 SMT.BGt a2
        _    -> Nothing
toBExp (PrimOp NotOp [e]) = do
    b <- toBExp e
    Just $ SMT.BNot b
toBExp _ = Nothing

mkArc :: Idx -> Idx -> Exp -> SCM (Maybe Arc)
mkArc i j e | Just a <- toAExp e = do
    conds <- asks scConditions
    lift $ lift $ do
        isStrict <- conds ==>? SMT.Bop (toAVar i) SMT.BGt a
        if isStrict then return $ Just $ Arc{arcFrom = i, arcTo = j, arcType = Strict}
        else do
            isNonStrict <- conds ==>? SMT.Bop (toAVar i) SMT.BGe a
            return $ if isNonStrict then Just $ Arc{arcFrom = i, arcTo = j, arcType = NonStrict}
            else Nothing
mkArc _ _ _ = return Nothing

buildSCGraphs :: Exp -> SCM ()
buildSCGraphs (Var _) = return ()
buildSCGraphs (Int _) = return ()
buildSCGraphs (PrimOp _ args) = mapM_ buildSCGraphs args
buildSCGraphs (Call fn args) = do
    sig@Sig{sigArity} <- asks scCallee
    let caller = Sig{sigName = fn, sigArity = length args}
    arcsss <- forM [0..sigArity-1] $ \i ->
            catMaybes <$> zipWithM (mkArc i) [0..sigArity-1] args
    addGraph $ SCGraph{scSource = sig, scTarget = caller, scArcs = S.fromList (concat arcsss)}
    mapM_ buildSCGraphs args
buildSCGraphs (Ite e1 e2 e3) = do
    buildSCGraphs e1
    case toBExp e1 of
        Nothing -> do
            buildSCGraphs e2
            buildSCGraphs e3
        Just b -> withCondition (SMT.bnot b) $ do
            buildSCGraphs e2
            buildSCGraphs e3
buildSCGraphs Error = return ()

mkSCGraphs :: Dec -> IO SCGraphSet
mkSCGraphs (Dec name arity value) = do
    let sig = Sig { sigName = name, sigArity = arity }
        ctx = SCContext { scCallee = sig, scConditions = [] }
    runSCM ctx $ do
        buildSCGraphs value
        get
