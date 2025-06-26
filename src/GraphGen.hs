{-# LANGUAGE ImpredicativeTypes #-}

module GraphGen
    ( mkSCGraphs
    ) where

import Graph
import Syntax
import Data.SBV
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

data SCContext = SCContext
    { scCallee      :: Sig
    , scSymbols     :: Symbolic [SInteger]
    , scConditions :: [Symbolic SBool]
    }

newtype SCState = SCState
    { scGraphs     :: [SCGraph]
    }

type SCM = ReaderT SCContext (StateT SCState (Either String))

runSCM :: SCContext -> SCM a -> Either String a
runSCM ctx action = evalStateT (runReaderT action ctx) initialState
  where
    initialState = SCState { scGraphs = [] }

addGraph :: SCGraph -> SCM ()
addGraph g = modify $ \s -> s { scGraphs = g : scGraphs s }

withCondition :: Symbolic SBool -> SCM a -> SCM a
withCondition c = local $ \s -> s { scConditions = c : scConditions s }

expToSInteger :: Exp -> Symbolic SInteger
expToSInteger (Var i) = sInteger $ "v" ++ show i
expToSInteger (Int n) = return (fromInteger n)
expToSInteger (PrimOp AddOp [e1, e2]) = (+) <$> expToSInteger e1 <*> expToSInteger e2
expToSInteger (PrimOp SubOp [e1, e2]) = (-) <$> expToSInteger e1 <*> expToSInteger e2
expToSInteger (PrimOp MulOp [e1, e2]) = (*) <$> expToSInteger e1 <*> expToSInteger e2
expToSInteger _ = fail "not an integer"

expToSBool :: Exp -> Symbolic SBool
expToSBool e@(PrimOp AddOp _) = sNot . (0 .==) <$> expToSInteger e
expToSBool e@(PrimOp SubOp _) = sNot . (0 .==) <$> expToSInteger e
expToSBool e@(PrimOp MulOp _) = sNot . (0 .==) <$> expToSInteger e
expToSBool (PrimOp AndOp [e1, e2]) = (.&&) <$> expToSBool e1 <*>  expToSBool e2
expToSBool (PrimOp OrOp [e1, e2]) = (.||) <$> expToSBool e1 <*>  expToSBool e2
expToSBool (PrimOp NotOp [e]) = sNot <$> expToSBool e
expToSBool (PrimOp EqOp [e1, e2]) = (.==) <$> expToSInteger e1 <*> expToSInteger e2
expToSBool (PrimOp GtOp [e1, e2]) = (.>) <$> expToSInteger e1 <*> expToSInteger e2
expToSBool _ = return sTrue

mkArc :: Idx -> Exp -> SCM [Arc]
mkArc i (Var j) = do
    return [ Arc
        { arcType = NonStrict
        , arcFrom = j
        , arcTo   = i
        } ]
mkArc i _exp = do
    return [ Arc
        { arcType = NonStrict
        , arcFrom = undefined
        , arcTo   = i
        } ]

goExp :: Exp -> SCM ()
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
    withCondition (expToSBool e1) $ do 
        goExp e2
        goExp e3

mkSCGraphs :: Dec -> Either String [SCGraph]
mkSCGraphs (Dec name arity value) = do
    let sig = Sig { sigName = name, sigArity = arity }
        syms = forM [1..arity] $ \i -> sInteger $ "v" ++ show i
        ctx = SCContext { scCallee = sig, scSymbols = syms, scConditions = [] }
    runSCM ctx $ do
        goExp value
        gets scGraphs
