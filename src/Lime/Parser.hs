module Lime.Parser
( astify
, Expr (Number, String, Atom)
) where

import Data.String.Utils
import Text.ParserCombinators.Parsec

data Expr = Number Int
          | String String
          | Atom String
          deriving (Show, Eq)

astify :: String -> Either ParseError Expr
astify = parse (parseExpr) "astify"

parseExpr :: Parser Expr
parseExpr = parseInt
        <|> parseString
        <|> parseAtom

parseInt :: Parser Expr
parseInt = do
             sign <- option "" $ string "-"
             n <- many1 digit
             return $ Number (read $ sign ++ n)

parseString :: Parser Expr
parseString = do
                char '"'
                s <- many $ chars
                char '"'
                return $ String s
  where chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escapedChar find repl)
        escapedChar x y = char x >> return y
        find = ['t',  'n',  '"', '\\']
        repl = ['\t', '\n', '"', '\\']

parseAtom :: Parser Expr
parseAtom = do
              head <- letter
              rest <- many (letter <|> digit <|> symbol)
              return $ Atom (head:rest)
  where symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
