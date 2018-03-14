{-# LANGUAGE MultiParamTypeClasses #-}

module ParseExpr where

import           Expr                       (Expr(..))
import           Control.Applicative        (many, some, (<|>))
import           Text.Megaparsec            (Parsec, runParser, between, notFollowedBy)
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec MyError String

data MyError
    = SomeInitianError
    | VarNotFound
    | VarHasBeenSet
    | DivByZero
    | ParseError
    deriving (Show, Eq, Ord)

sc :: Parser ()
sc = L.space Text.Megaparsec.Char.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '\''))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

varParser :: Parser Expr
varParser = Var <$> identifier

atomParser :: Parser Expr
atomParser  =   parens exprParser
            <|> varParser

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

lambdaParser :: Parser Expr
lambdaParser = do
    _ <- symbol "\\"
    i <- identifier
    _ <- symbol "."
    a <- exprParser
    return (Lambda i a)

exprParser :: Parser Expr
exprParser = foldl1 Application <$> some (atomParser <|> lambdaParser)

myRun :: Parser Expr -> String -> Expr
myRun p s = either (\_ -> Var "Error") id (runParser p "" s)

parseExpr :: String -> Expr
parseExpr = myRun exprParser
