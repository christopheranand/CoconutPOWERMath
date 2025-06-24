module TestPow(test,powDbg,powT10,powT1,powT2) where
-- import qualified Data.List as List
import Pow
import TestUtils
import Coconut.BaseTypes
import ISA.PowerISA
-- import ISA.PowerInterp
-- import MathUtils
-- import Data.List
-- import PrelExts
import Test.Hspec
-- import Control.Monad
-- import Data.Foldable

-- version = "$Id: TestSqrt.lhs 1205 2006-09-18 12:36:58Z anand $"

test :: IO ()
test = hspec $ do
  describe "Pow zero" $ do
    it "powSP 0 0 = 1" $ do
      floats @Interp (powSP (unfloats4 0) (unfloats4 0)) `shouldBe` [1.0, 1.0, 1.0, 1.0]
  describe "Pow negative exponent" $ do
    it "powSP 1 -100 = 1" $ do
      floats @Interp (powSP (unfloats4 1) (unfloats4 (-100))) `shouldBe` [1.0, 1.0, 1.0, 1.0]
  describe "Pow subnormal" $ do
    it "powSP 1.40E-45 1 = 1" $ do
      floats @Interp (powSP (unfloats4 (1.40E-45)) (unfloats4 (1))) `shouldBe` [0.0, 0.0, 0.0, 0.0]
    it "powSP 1.40E-45 -1 = 1/0" $ do
      floats @Interp (powSP (unfloats4 (1.40E-45)) (unfloats4 (-1))) `shouldBe` [1/0, 1/0, 1/0, 1/0]
    it "powSP 0 1.40E-45 = 0" $ do
      floats @Interp (powSP (unfloats4 (0)) (unfloats4 (1.40E-45))) `shouldBe` [0.0, 0.0, 0.0, 0.0]
  describe "Pow NaN" $ do
    it "powSP NaN 0 = 1" $ do
      floats @Interp (powSP (unfloats4 (0/0)) (unfloats4 (0))) `shouldBe` [1.0, 1.0, 1.0, 1.0]
    it "powSP 0 NaN = NaN" $ do
     (head $ floats @Interp (powSP (unfloats4 (0)) (unfloats4 (0/0)))) `shouldSatisfy` Prelude.isNaN 
    it "powSP 1/0 -1/0 = 0" $ do
      floats @Interp (powSP (unfloats4 (1/0)) (unfloats4 (-1/0))) `shouldBe` [0.0, 0.0, 0.0, 0.0]
    it "powSP -1/0 1/0 = 1/0" $ do
      floats @Interp (powSP (unfloats4 (-1/0)) (unfloats4 (1/0))) `shouldBe` [1/0, 1/0, 1/0, 1/0]

powDbg :: [Double] -> [Double] -> IO ()
powDbg bases powers = 
  let
    result = snd $ powDev (unfloats bases) (unfloats powers)
  in 
    case result of
      (input : rest) -> do
        let answer = ("Haskell fun", unfloats $ zipWith (\x y -> x ** y) bases powers)
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
      _ -> putStrLn "Empty list: Nothing to process."
      
      

powT10 :: (Double, Double)
powT10 = worstError 
   $ cmpFltToHaskellFun2 powSP (\ x -> \ y -> x ** y) [ (x/78,y/7) | x<-[1..197], y<-[-8..8]]

powT1 :: (Double, Double)
powT1 = worstError 
   $ cmpFltToHaskellFun2 powSP (\ x -> \ y -> x ** y) [ (x/78,y/7) | x<-[1..97], y<-[-2..2]]

powT2 :: [String]
powT2 = testf2 powSP 2 2
