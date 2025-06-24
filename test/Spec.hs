module Spec where

import Test.Hspec

import Exp
import ISA.PowerISA
import ISA.PowerInterp
import Coconut.BaseTypes
import TestUtils
import Control.Monad
import Data.Foldable

main :: IO ()
main = hspec $ do
  describe "Exp tests" $ do
    it "expSP 0 = 1" $ do
      floats @Interp (expSP (unfloats4 0)) `shouldBe` [1.0, 1.0, 1.0, 1.0] 
    it "expSP 0 = 1 within tolerance" $ do
      floats @Interp (expSP (unfloats4 0))
        `shouldSatisfy`
        (all (\n -> 0.1 <= n && n <= 0.9))
    sequence_ $
      map (fmatSpec "expm1SP" expm1SP (\x -> exp x - 1) 1 (1/2)) $
        [i/43 | i<-[-47..47]]++[3/4+1/64,-(3/4+1/64)]

