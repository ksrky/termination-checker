{-|
Parser for mixfix operators.
-}
module Parser.OperatorParser
    ( insertMixfixParser
    , mkParserTable
    , infixOp
    , infixlOp
    , infixrOp
    , prefixOp
    , postfixOp
    ) where

import Parser.Logic
import Parser.Monad
import Parser.Types

-- | Build a parser from Operator/Operand list.
parseOpers :: [Oper] -> Parser
parseOpers [] _ = id
parseOpers (Operator t : opers) ctx = parseOpers opers ctx . matchToken (Token t ==) ctx
parseOpers (Operand bp : opers) ctx = parseOpers opers ctx . parseLeading ctx{bindingPower = bp}

-- | Insert a parser for a mixfix operator into a parser table.
insertMixfixParser :: MixfixOp -> ParserTable -> ParserTable
insertMixfixParser mop@(Operator t0 : opers) = do
    let arity = length [() | Operand{} <- opers]
        parser = parseOpers opers >>> const (mkApp mop arity)
    insertLeadingParser t0 parser
insertMixfixParser mop@(Operand bp0 : Operator t1 : opers) = do
    let arity = 1 + length [() | Operand{} <- opers]
        parser ctx
            | bp0 < bindingPower ctx = \st -> st{errorMsg = Just "lower binding power"}
            | otherwise = (shift >>> parseOpers opers >>> const (mkApp mop arity) >>> parseTrailing) ctx
    insertTrailingParser t1 parser
insertMixfixParser _ = error "invalid mixfix op: an operator does not appear at the first or second position."

-- | Create a parser table from a list of mixfix operators.
mkParserTable :: [MixfixOp] -> ParserTable
mkParserTable = foldr insertMixfixParser initParserTable

-- | Operator/Operand list of infix parsers.
infixOp :: BindingPower -> T -> [Oper]
infixOp bp  t = [Operand bp, Operator t, Operand bp]

-- | Operator/Operand list of infixl parsers.
infixlOp :: BindingPower -> T -> [Oper]
infixlOp bp  t = [Operand (succ bp), Operator t, Operand bp]

-- | Operator/Operand list of infixr parsers.
infixrOp :: BindingPower -> T -> [Oper]
infixrOp bp  t = [Operand bp, Operator t, Operand (succ bp)]

-- | Operator/Operand list of prefix parsers.
prefixOp :: BindingPower -> T -> [Oper]
prefixOp bp  t = [Operator t, Operand bp]

-- | Operator/Operand list of postfix parsers.
postfixOp :: BindingPower -> T -> [Oper]
postfixOp bp  t = [Operand bp, Operator t]
