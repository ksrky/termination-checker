{-# LANGUAGE ImpredicativeTypes #-}

module GraphGen where

import Graph
import Syntax
import Data.SBV
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

data SCContext = SCContext
    { scCallee     :: Sig
    , scSymbols    :: Symbolic [SInteger]
    , scConditions :: [[SInteger] -> Maybe SBool]
    }

newtype SCState = SCState
    { scGraphs :: [SCGraph]
    }

data ControlTree
    = Constraint Exp [ControlTree]
    | Problem Ident [Exp]
    deriving (Show)

type SCM = ReaderT SCContext (StateT SCState (Either String))

runSCM :: SCContext -> SCM a -> Either String a
runSCM ctx action = evalStateT (runReaderT action ctx) initialState
  where
    initialState = SCState { scGraphs = [] }

addGraph :: SCGraph -> SCM ()
addGraph g = modify $ \s -> s { scGraphs = g : scGraphs s }

withCondition :: ([SInteger] -> Maybe SBool) -> SCM a -> SCM a
withCondition c = local $ \s -> s { scConditions = c : scConditions s }

findVar :: [SInteger] -> Int -> Maybe SInteger
findVar [] _ = error "Out of scope"
findVar (v : _) 1 = Just v
findVar (_ : vs) n = findVar vs (n - 1)

expToSInteger :: [SInteger] -> Exp -> Maybe SInteger
expToSInteger vs (Var i) = findVar vs i
expToSInteger _ (Int n) = Just $ fromInteger n
expToSInteger vs (PrimOp AddOp [e1, e2]) = (+) <$> expToSInteger vs e1 <*> expToSInteger vs e2
expToSInteger vs (PrimOp SubOp [e1, e2]) = (-) <$> expToSInteger vs e1 <*> expToSInteger vs e2
expToSInteger vs (PrimOp MulOp [e1, e2]) = (*) <$> expToSInteger vs e1 <*> expToSInteger vs e2
expToSInteger _ _ = Nothing

expToSBool :: [SInteger] -> Exp -> Maybe SBool
expToSBool vs (Var i) = do
    v <- findVar vs i
    Just $ sNot $ v .== 0
expToSBool vs e@(PrimOp AddOp _) = sNot . (0 .==) <$> expToSInteger vs e
expToSBool vs e@(PrimOp SubOp _) = sNot . (0 .==) <$> expToSInteger vs e
expToSBool vs e@(PrimOp MulOp _) = sNot . (0 .==) <$> expToSInteger vs e
expToSBool vs (PrimOp AndOp [e1, e2]) = (.&&) <$> expToSBool vs e1 <*>  expToSBool vs e2
expToSBool vs (PrimOp OrOp [e1, e2]) = (.||) <$> expToSBool vs e1 <*>  expToSBool vs e2
expToSBool vs (PrimOp NotOp [e]) = sNot <$> expToSBool vs e
expToSBool vs (PrimOp EqOp [e1, e2]) = (.==) <$> expToSInteger vs e1 <*> expToSInteger vs e2
expToSBool vs (PrimOp GtOp [e1, e2]) = (.>) <$> expToSInteger vs e1 <*> expToSInteger vs e2
expToSBool _ _ = Nothing

mkArc :: Idx -> Exp -> SCM [Arc]
mkArc i (Var j) =
    return [ Arc
        { arcType = NonStrict
        , arcFrom = j
        , arcTo   = i
        } ]
mkArc i _exp =
    return [ Arc
        { arcType = NonStrict
        , arcFrom = undefined
        , arcTo   = i
        } ]

mkControlTrees :: Exp -> [ControlTree]
mkControlTrees (Var _) = []
mkControlTrees (Int _) = []
mkControlTrees (Call fn args) = Problem fn args : concatMap mkControlTrees args
mkControlTrees (PrimOp _ args) = concatMap mkControlTrees args
mkControlTrees (Ite e1 e2 e3) = [Constraint e1 (mkControlTrees e2), Constraint (PrimOp NotOp [e1]) (mkControlTrees e3)]

{- goExp :: Exp -> SCM ()
goExp (Var _) = return ()
goExp (Int _) = return ()
goExp (Call fn args) = do
    sig <- asks scCallee
    arcs <- concat <$> zipWithM mkArc [1..] args
    let scCaller = Sig {sigName = fn, sigArity = length args} 
    addGraph $ SCGraph{scSource = sig, scTarget = scCaller, scArcs = arcs}
    mapM_ goExp args
goExp (PrimOp _ args) = mapM_ goExp args
goExp (Ite e1 e2 e3) = do
    goExp e1
    withCondition (flip expToSBool e1) $ do 
        goExp e2
        goExp e3
-}

mkSCGraphs :: Dec -> Either String [SCGraph]
mkSCGraphs (Dec name arity value) = do
    let trees = mkControlTrees value
    let sig = Sig { sigName = name, sigArity = arity }
        syms = forM [1..arity] $ \i -> sInteger $ "v" ++ show i
        ctx = SCContext { scCallee = sig, scSymbols = syms, scConditions = [] }
    runSCM ctx $ gets scGraphs
