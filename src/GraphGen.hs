{-# LANGUAGE ImpredicativeTypes #-}

module GraphGen (mkSCGraphs) where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Graph
import qualified SMT.Syntax           as SMT
import           SMT.ToSymbolic
import           Syntax

data SCContext = SCContext
    { scCallee     :: Sig
    , scConditions :: [SMT.BExp]
    }

newtype SCState = SCState
    { scGraphs :: [SCGraph]
    }

type SCM = ReaderT SCContext (StateT SCState IO)

runSCM :: SCContext -> SCM a -> IO a
runSCM ctx action = evalStateT (runReaderT action ctx) initialState
  where
    initialState = SCState { scGraphs = [] }

addGraph :: SCGraph -> SCM ()
addGraph g = modify $ \s -> s { scGraphs = g : scGraphs s }

withCondition :: SMT.BExp -> SCM a -> SCM a
withCondition c = local $ \s -> s { scConditions = c : scConditions s }

mkArc :: Idx -> Idx -> Exp -> SCM [Arc]
mkArc i j e | Just a <- toAExp e = do
    conds <- asks scConditions
    lift $ lift $ do
        isStrict <- proveWithAssump conds (SMT.Bop (toAVar i) SMT.BGt a)
        isNonStrict <- proveWithAssump conds (SMT.Bop (toAVar i) SMT.BGe a)
        return $ if isStrict then [Arc Strict i j]
            else [Arc NonStrict i j | isNonStrict]
mkArc _ _ _ = return []

buildSCGraphs :: Exp -> SCM ()
buildSCGraphs (Var _) = return ()
buildSCGraphs (Int _) = return ()
buildSCGraphs (PrimOp _ args) = mapM_ buildSCGraphs args
buildSCGraphs (Call fn args) = do
    sig@Sig{sigArity} <- asks scCallee
    let caller = Sig{sigName = fn, sigArity = length args}
    arcsss <- forM [1..sigArity] $ \i ->
            zipWithM (mkArc i) [1..sigArity] args
    addGraph $ SCGraph{scSource = sig, scTarget = caller, scArcs = concat (concat arcsss)}
    mapM_ buildSCGraphs args
buildSCGraphs (Ite e1 e2 e3) = do
    buildSCGraphs e1
    case toBExp e1 of
        Nothing -> do
            buildSCGraphs e2
            buildSCGraphs e3
        Just b -> withCondition b $ do
            buildSCGraphs e2
            buildSCGraphs e3

mkSCGraphs :: Dec -> IO [SCGraph]
mkSCGraphs (Dec name arity value) = do
    let sig = Sig { sigName = name, sigArity = arity }
        ctx = SCContext { scCallee = sig, scConditions = [] }
    runSCM ctx $ do
        buildSCGraphs value
        gets scGraphs

toAVar :: Idx -> SMT.AExp
toAVar i = SMT.AVar ("x" ++ show i)

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
