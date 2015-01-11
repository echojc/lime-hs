module Lime.Parser
( astify
, Expr (Number, String, Bool, Atom, List)
) where

import Data.String.Utils
import Text.ParserCombinators.Parsec

data Expr = Number Int
          | String String
          | Bool Bool
          | Atom String
          | List [Expr]
          deriving (Show, Eq)

astify :: String -> Either ParseError Expr
astify = parse (parseExpr) "astify"

parseExpr :: Parser Expr
parseExpr = parseInt
        <|> parseString
        <|> parseAtom
        <|> parseList
        <|> parseQuoted

parseQuoted :: Parser Expr
parseQuoted = do
                char '\''
                expr <- parseExpr
                return $ List [Atom "quote", expr]

parseList :: Parser Expr
parseList = do
              char '('
              exprs <- sepBy parseExpr whitespace
              char ')'
              return $ List exprs
  where whitespace = skipMany1 space

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
              return $ case (head:rest) of
                "true" -> Bool True
                "false" -> Bool False
                x -> Atom x
  where symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

