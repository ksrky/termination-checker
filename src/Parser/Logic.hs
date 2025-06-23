{-|
Core logic of P3 algorithm.
-}
module Parser.Logic
    ( parseLeading
    , parseTrailing
    ) where

import Data.List                qualified as L
import Parser.Monad
import Parser.Types

longestMatch :: [Parser] -> Parser
longestMatch parsers ctx st =
    let sts = tryParsers parsers ctx st in
    case L.sortOn (negate . position) sts of
        []     -> st{errorMsg = Just "No match parsers"}
        st': _ -> st'

tryParsers :: [Parser] -> ParserContext -> ParserState -> [ParserState]
tryParsers parsers ctx st = filter (not . hasError) $ map (\p -> p ctx st) parsers

parseLeading :: Parser
parseLeading ctx st
    | hasError st2
    , Token (Letter _) <- tok
    , let st3 = mkVar tok ctx st2
    = parseTrailing ctx st3
    | otherwise
    = parseTrailing ctx st2
  where
    tok = peek st
    st1 = shift ctx st
    st2 = longestMatch (leadingParsersOf ctx tok) ctx st1

parseTrailing :: Parser
parseTrailing ctx st = recoverError $ longestMatch (trailingParsersOf ctx (peek st)) ctx st
