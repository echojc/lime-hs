{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Lime.Codegen
( compile
) where

import Lime.Parser

import Control.Monad
import Control.Monad.Exception
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack)

import JVM.Assembler
import JVM.Builder
import JVM.ClassFile
import JVM.Converter
import JVM.Exceptions
import qualified Java.Lang

compile :: B.ByteString -> Expr -> Class Direct
compile className expr = generate [] className $ do
  -- ctor, required
  newMethod [ACC_PUBLIC] "<init>" [] ReturnsVoid $ do
    aload_ I0
    invokeSpecial Java.Lang.object Java.Lang.objectInit
    i0 RETURN

  -- compile each fun
  case expr of
    List [Atom "fn", List (Atom methodName : args), body] -> compileFun (pack methodName) body

  return ()

compileFun :: (Generator e g, Throws UnexpectedEndMethod e)
           => B.ByteString
           -> Expr
           -> g e (NameType (Method Direct))
compileFun methodName body =
  newMethod [ACC_PUBLIC, ACC_STATIC]
            methodName
            [arrayOf $ ObjectType Java.Lang.object]
            (Returns $ ObjectType Java.Lang.object)
            $ do
    compileExpr body
    i0 ARETURN

compileExpr :: (Generator e g) => Expr -> g e ()
compileExpr expr = case expr of
  Number n      -> ldc2 $ CInteger (fromIntegral n)
  Atom "+"      -> iadd
  List (f:args) -> foldl1 (>>) (map compileExpr $ args ++ [f])
