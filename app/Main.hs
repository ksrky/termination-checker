module Main (main) where

import Data.Set qualified as S
import SCGraph
import FlowAnalysis
import Lexer
import Parser
import SCT

main :: IO ()
main = do
    s <- getContents
    let decs = parse (alexScanTokens s)
    scgset <- S.unions <$> mapM mkSCGraphs decs
    let fns = checkTermination scgset
    print $ map sigName fns 
