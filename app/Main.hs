module Main (main) where

import Lexer
import Parser

main :: IO ()
main = do
    s <- getContents
    let ds = parse (alexScanTokens s)
    print ds
