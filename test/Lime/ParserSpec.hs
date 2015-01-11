module Lime.ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.ParserCombinators.Parsec.Error (ParseError, errorMessages)

import Data.String.Utils
import Lime.Parser

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

spec :: Spec
spec = do
  describe "lime parser" $ do
    it "parses numbers" $ property $ \x ->
      astify (show x) `shouldBe` Right (Number x)

    it "parses strings" $ property $ \x ->
      astify (escapeString x) `shouldBe` Right (String x)

    it "parses atoms" $ forAll validAtoms $ \x ->
      astify x `shouldBe` Right (Atom x)

----

escapeString :: String -> String
escapeString = quote . escape
  where quote s = "\"" ++ s ++ "\""
        escape s = foldr ($) s (zipWith replace find repl)
        find = ["\t",  "\n",  "\"",   "\\"]
        repl = ["\\t", "\\n", "\\\"", "\\\\"]

validAtoms :: Gen String
validAtoms = do
               head <- elements letter
               rest <- listOf $ elements $ letter ++ digit ++ symbol
               return $ head:rest
  where letter = ['a'..'z'] ++ ['A'..'Z']
        digit = ['0'..'9']
        symbol = "!#$%&|*+-/:<=>?@^_~"
