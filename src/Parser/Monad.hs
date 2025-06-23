{-# LANGUAGE DerivingVia     #-}

{-|
Contexts, states and monads for P3 parser.
-}
module Parser.Monad
    ( -- * Data types for parser monad
      -- ** Parser context
      ParserContext (..)
    , initParserContext
      -- ** Parser state
    , ParserState (..)
    , initParserState
    , hasError
    , recoverError
    , throwError
      -- ** Parser table
    , ParserTable (..)
    , initParserTable
    , insertLeadingParser
    , insertTrailingParser
    , leadingParsersOf
    , trailingParsersOf
    , Parser
    , (>>>)
    , runParser
    , shift
    , matchToken
    , eot
    , mkVar
    , mkApp
    ) where

import Data.Map.Strict qualified as M
import Parser.Types

-- | Reader context for parser.
data ParserContext = ParserContext
    { bindingPower :: BindingPower  -- ^ Current binding power.
    , parserTable  :: ParserTable   -- ^ Parser table indexed by tokens.
    }

initParserContext :: ParserContext
initParserContext = ParserContext
    { bindingPower = minBound
    , parserTable  = initParserTable
    }

-- | State for parser.
data ParserState = ParserState
    { stxStack :: SyntaxStack     -- ^ Stored parse results.
    , peek     :: Token         -- ^ Next token to be processed. `Nothing` means the end of the token stream.
    , tokens   :: [Token]       -- ^ Input token stream.
    , position :: Int           -- ^ Position in the token stream.
    , errorMsg :: Maybe String  -- ^ Error message, if any.
    }

initParserState :: [T] -> ParserState
initParserState ts = ParserState
    { stxStack = []
    , peek
    , tokens
    , position = 0
    , errorMsg = Nothing
    }
  where
    (peek, tokens) = case ts of
        []     -> (Terminator, [])
        x : xs -> (Token x, map Token xs ++ [Terminator])

hasError :: ParserState -> Bool
hasError st = errorMsg st /= Nothing

recoverError :: ParserState -> ParserState
recoverError st = st{errorMsg = Nothing}

throwError :: String -> ParserState -> ParserState
throwError msg st = st{errorMsg = Just msg}

type Parser = ParserContext -> ParserState -> ParserState

infixl 9 >>>

(>>>) :: Parser -> Parser -> Parser
(>>>) p1 p2 ctx = p2 ctx . p1 ctx

-- | A table containing leading and trailing parsers indexed by tokens.
data ParserTable = ParserTable
    { leadingParsers  :: M.Map T [Parser]
    , trailingParsers :: M.Map T [Parser]
    }

initParserTable :: ParserTable
initParserTable = ParserTable
    { leadingParsers  = M.empty
    , trailingParsers = M.empty
    }

insertLeadingParser :: T -> Parser -> ParserTable -> ParserTable
insertLeadingParser t p tbl = tbl{leadingParsers = M.insertWith (++) t [p] (leadingParsers tbl)}

insertTrailingParser :: T -> Parser -> ParserTable -> ParserTable
insertTrailingParser t p tbl = tbl{trailingParsers = M.insertWith (++) t [p] (trailingParsers tbl)}

leadingParsersOf :: ParserContext -> Token -> [Parser]
leadingParsersOf ctx (Token t) = concat $ leadingParsers (parserTable ctx) M.!? t
leadingParsersOf _ Terminator = []

trailingParsersOf :: ParserContext -> Token -> [Parser]
trailingParsersOf ctx (Token t) = concat $ trailingParsers (parserTable ctx) M.!? t
trailingParsersOf _ Terminator = []

runParser :: ParserTable -> Parser -> [T] -> Either String Syntax
runParser tbl parser ts =
    let st = parser initParserContext{parserTable = tbl} (initParserState ts) in
    case (errorMsg st, stxStack st) of
        (Just msg, _)    -> Left msg
        (Nothing, [stx]) -> Right stx
        (Nothing, _)     -> error "runParser: invalid sytax stack"

-- | Update the current token and position in the parser state.
shift :: Parser
shift _ st = do 
    case tokens st of
        x : xs -> st{peek = x, tokens = xs, position = position st + 1}
        []     -> st

-- | Match the current token with a predicate and shift the state if it matches.
matchToken :: (Token -> Bool) -> Parser
matchToken p ctx st = do 
    if p (peek st)
        then shift ctx st
        else st{errorMsg = Just "No match parsers"}

eot :: Parser
eot _ st | peek st == Terminator = st
         | otherwise = st{errorMsg = Just "Expected the end of the token stream"}

-- | Push a syntax node to the syntax stack.
pushSyntax :: Syntax -> ParserState -> ParserState
pushSyntax stx st = st{stxStack = stx : stxStack st}

-- | Push `Atom` to the syntax stack.
mkVar :: Token -> Parser
mkVar (Token t) _ = pushSyntax (Var t)
mkVar Terminator _ = id

-- | Push `Node` to the syntax stack. Reduce operation.
mkApp :: MixfixOp -> Int -> ParserState -> ParserState
mkApp _ n _ | n < 0 = error "mkNode: negative arity"
mkApp mop n st | n > length (stxStack st) = st{errorMsg = Just $ "Not enough syntax stack for " ++ show mop}
mkApp mop n st = 
    let (stxs, rest) = splitAt n (stxStack st) in
    st{stxStack = App mop (reverse stxs) : rest}
