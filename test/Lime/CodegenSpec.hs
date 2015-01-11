{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Lime.CodegenSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Lime.Codegen
import Lime.Parser

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Word

import JVM.Assembler
import JVM.ClassFile
import JVM.Common
import JVM.Converter
import qualified Java.Lang

spec :: Spec
spec = do
  it "compiles an add function" $ do
    let expr = List [Atom "fn", List [Atom "foo"], List [Atom "+", Number 1, Number 2]]
    let clazz = compile "Test" expr
    ins clazz "foo" `shouldBe` Just [ LDC2 $ intLookup clazz 1
                                    , LDC2 $ intLookup clazz 2
                                    , IADD
                                    , ARETURN
                                    ]


ins :: (Class Direct) -> B.ByteString ->  Maybe [Instruction]
ins clazz methodName = liftM (codeInstructions . decodeMethod) $ methodCode clazz methodName

intLookup :: (Class Direct) -> Int -> Word16
intLookup clazz int = fromJust $ mapFindIndex eqConst pool
  where eqConst = (== CInteger (fromIntegral int))
        pool = constsPool clazz
