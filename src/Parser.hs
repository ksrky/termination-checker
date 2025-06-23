module Parser (pProg) where

import Data.Char
import Parser.Logic
import Parser.Monad
import Parser.OperatorParser
import Parser.Types
import Term
import Text.ParserCombinators.ReadP

isMixfixOpChar :: Char -> Bool
isMixfixOpChar c = isPrint c && not (isSpace c) && c `notElem` [':', '=', '.', '(', ')']

isOpChar :: Char -> Bool
isOpChar c = isPrint c && not (isSpace c) && c `notElem` [':', '=', '.', '(', ')', '_']

pId :: ReadP String
pId = (:) <$> satisfy isAlpha <*> munch1 isAlphaNum

pOp :: ReadP String
pOp = munch1 isOpChar

pMixfixOp :: ReadP String
pMixfixOp = munch1 isMixfixOpChar

letter :: ReadP T
letter = Letter <$> pId

symbol :: ReadP T
symbol = Symbol <$> pOp

pToken :: ReadP T
pToken = letter <++ symbol

pTokens :: ReadP [T]
pTokens = many $ pToken <* skipSpaces

pInt :: ReadP Int
pInt = read <$> munch1 isDigit

pBindingPower :: ReadP BindingPower
pBindingPower = do
    n <- pInt
    if minBound <= n && n <= maxBound
        then return $ BindingPower n
        else pfail

pType :: ReadP Type
pType = (Arrow <$> pType1 <* string "->" <*> pType) <++ pType1

pType1 :: ReadP Type
pType1 = (Type <$ char '*') +++ between (char '(') (char ')') pType

pDecl :: ReadP Command
pDecl = do
    name <- pMixfixOp
    _ <- string ":"
    Decl name <$> pType

pEqn :: ParserTable -> ReadP Command
pEqn tbl = do
    t1 <- pSyntax tbl
    skipSpaces
    _ <- char '='
    skipSpaces
    t2 <- pSyntax tbl
    return $ Eqn t1 t2

pSyntax :: ParserTable -> ReadP Term
pSyntax tbl = do
    ts <- pTokens
    case runParser tbl (parseLeading <* eot) ts of
        Left _ -> pfail
        Right stx -> return $ stxToTerm stx
  where
    stxToTerm :: Syntax -> Term
    stxToTerm (Var t) = Ne (Fvar (show t)) []
    stxToTerm (App mop args) = Ne (Fvar (pprMixfixOp mop)) (map stxToTerm args)

    pprMixfixOp :: MixfixOp -> String
    pprMixfixOp = concatMap $ \case
        Operator (Letter s) -> s
        Operator (Symbol s) -> s
        Operand _  -> "_"

pCommand :: ParserTable -> ReadP Command
pCommand tbl = (pDecl +++ pEqn tbl) <* char '.'

pFixityDecl :: ReadP MixfixOp
pFixityDecl = do
    fixity <- string "infix" +++ string "infixl" +++ string "infixr" +++ string "prefix" +++ string "postfix" +++ string "mixfix"
    bp <- pBindingPower
    case fixity of
        "infix"   -> infixOp bp <$> pToken
        "infixl"  -> infixlOp bp <$> pToken
        "infixr"  -> infixrOp bp <$> pToken
        "prefix"  -> prefixOp bp <$> pToken
        "postfix" -> postfixOp bp <$> pToken
        "mixfix"  -> mixfixOp bp
        _         -> pfail
  where
    mixfixOp :: BindingPower -> ReadP MixfixOp
    mixfixOp bp = ((:) <$> (Operand bp <$ char '_') <*> mixfixOp bp)
        +++ (((:) . Operator <$> pToken) <*> mixfixOp bp) <++ return []

pCommands :: ParserTable -> ReadP [Command]
pCommands tbl = ((:) <$> pCommand tbl <*> pCommands tbl)
    +++ (pFixityDecl >>= pCommands . flip insertMixfixParser tbl)

pProg :: ReadP [Command]
pProg = pCommands initParserTable
