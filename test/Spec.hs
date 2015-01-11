-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Lime.ParserSpec
import qualified Lime.CodegenSpec

main :: IO ()
main = hspec $ do
  -- describe "Lime.ParserSpec"  Lime.ParserSpec.spec
  describe "Lime.CodegenSpec" Lime.CodegenSpec.spec
