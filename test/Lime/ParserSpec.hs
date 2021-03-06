module Lime.ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.ParserCombinators.Parsec.Error (ParseError, errorMessages)

import Control.Monad
import Data.List
import Data.String.Utils
import Lime.Parser

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

spec :: Spec
spec = do
  describe "lime parser" $ do
    it "parses numbers" $ forAll numberGen verifyAst
    it "parses strings" $ forAll stringGen verifyAst
    it "parses bools" $ forAll boolGen verifyAst
    it "parses atoms" $ forAll atomGen verifyAst
    it "parses lists" $ forAll listGen verifyAst
    it "parses nested lists" $ forAll nestedListGen verifyAst
    it "parses quotes" $ forAll quotedGen verifyAst

verifyAst :: (String, Expr) -> IO ()
verifyAst (code, ast) = astify code `shouldBe` Right ast

numberGen :: Gen (String, Expr)
numberGen = do
              num <- arbitrary
              return (show num, Number num)

stringGen :: Gen (String, Expr)
stringGen = do
              str <- arbitrary
              return (escapeStr str, String str)
  where escapeStr = quote . escape
        quote s = "\"" ++ s ++ "\""
        escape s = foldr ($) s (zipWith replace find repl)
        find = ["\t",  "\n",  "\"",   "\\"]
        repl = ["\\t", "\\n", "\\\"", "\\\\"]

boolGen :: Gen (String, Expr)
boolGen = elements [("true", Bool True), ("false", Bool False)]

atomGen :: Gen (String, Expr)
atomGen = do
            head <- elements letter
            rest <- listOf $ elements $ letter ++ digit ++ symbol
            let atom = head : rest
            return (atom, Atom atom)
  where letter = ['a'..'z'] ++ ['A'..'Z']
        digit = ['0'..'9']
        symbol = "!#$%&|*+-/:<=>?@^_~"

listGen :: Gen (String, Expr)
listGen = listGen' [numberGen, stringGen, atomGen]

-- we'll test one level of nesting
nestedListGen :: Gen (String, Expr)
nestedListGen = listGen' [numberGen, stringGen, atomGen, listGen]

listGen' :: [Gen (String, Expr)] -> Gen (String, Expr)
listGen' gens = do
            (codes, asts) <- liftM unzip $ listOf (oneof gens)
            return (stringify codes, List asts)
  where stringify x = "(" ++ (concat $ intersperse " " x) ++ ")"

quotedGen :: Gen (String, Expr)
quotedGen = do
             (code, ast) <- oneof [numberGen, stringGen, atomGen, listGen]
             return ('\'':code, List [Atom "quote", ast])
