module TestSinc(test, sincTest1, sincTest2, sincDbg) where

import TestUtils
import Coconut.BaseTypes
import ISA.PowerISA
import Sinc
import Test.Hspec


test :: IO () 
test = hspec $ do
  -- test sincSP
  describe "sincSP zero" $ do
    it "sincSP 0 = 0/0" $ do
      (head $ floats @Interp (sincSP (unfloats4 (0)))) `shouldSatisfy` Prelude.isNaN
  describe "sincSP infinity" $ do
    it "sincSP 1/0 = 0/0" $ do
      (head $ floats @Interp (sincSP (unfloats4 (1/0)))) `shouldSatisfy` Prelude.isNaN
  describe "sincSP -infinity" $ do
    it "sincSP -1/0 = 0/0" $ do
      (head $ floats @Interp (sincSP (unfloats4 (-1/0)))) `shouldSatisfy` Prelude.isNaN
  describe "sincSP NaN" $ do
    it "sincSP 0/0 = 0/0" $ do
      (head $ floats @Interp (sincSP (unfloats4 (0/0)))) `shouldSatisfy` Prelude.isNaN
  describe "sincSP negative number" $ do
    it "sincSP -1 = 0.8414709568023682" $ do
      floats @Interp (sincSP (unfloats4 (-1))) `shouldSatisfy`
        (all (\n -> (0.8414709568023682 - (5.95 * 10**(-7))) <= n && n <= (0.8414709568023682 + (5.95 * 10**(-7)))))
  describe "sincSP subnormal" $ do
    it "sincSP 1.4E-45 = 1" $ do
      floats @Interp (sincSP (unfloats4 (1.4E-45))) `shouldBe` [1.0, 1.0, 1.0, 1.0]
    it "sincSP -1.4E-45 = 1" $ do
      floats @Interp (sincSP (unfloats4 (-1.4E-45))) `shouldBe` [1.0, 1.0, 1.0, 1.0]
  
sincTest1 :: IO ()
sincTest1 = fmat sincSP (\ x -> (sin x) / x) 10 (1/2) [ 1+x/1024 | x<-([-283..283])]

sincTest2 :: IO ()
sincTest2 = fmat sincSP (\ x -> (sin x) / x) 10 (1/2) [ 1+x/33 | x<-([-283..283])]

sincDbg :: [Double] -> IO ()
sincDbg fourFloats =
  let
    result = snd $ sincDev (unfloats @Interp fourFloats)
  in case result of
    (input : rest) -> 
      let
        answer = ("Haskell fun", unfloats $ map (\x -> (sin x) / x) $ floats @Interp (unfloats @Interp fourFloats))
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
    _ -> putStrLn "Empty list: Nothing to process."


-- sincTest1 = fmat sincSP (\ x -> (sin x) / x) 10 (1/2) [ 1+x/1024 | x<-([-283..283])]

-- sincTest2 = fmat sincSP (\ x -> (sin x) / x) 10 (1/2) [ 1+x/33 | x<-([-283..283])]

-- sincTest1 = fmat sincSP (\ x -> (sin x) / x) 10 (1/2) [ 1+x/1024 | x<-([0])]

-- sincTest2 = fmat sincSP (\ x -> (sin x) / x) 10 (1/2) [ 1+x/33 | x<-([-283..283])]

-- sinc2D1 = fmat sincSP (\ x -> ((sin x) / x)) 10 (1/2) [ 1+x/1024 | x<-([-82..283])]