module Main (main) where

import           GraphGen
import           Lexer
import           Parser

main :: IO ()
main = do
    s <- getContents
    let ds = parse (alexScanTokens s)
    gs <- mapM mkSCGraphs ds
    print $ length gs
