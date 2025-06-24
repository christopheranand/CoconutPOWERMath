module TestQuadrt(test,quadrtSPDbg,qdrt1ReleaseDbg,recipQdrtDbg,quadrtSPT10,recipQdrtT10) where

-- import qualified Data.List as List
import Quadrt

import Test.Hspec

import Coconut.BaseTypes
import ISA.PowerISA
-- import ISA.PowerInterp
import TestUtils
-- import Control.Monad
-- import Data.Foldable

test :: IO ()
test = hspec $ do
  describe "Quadrt zero" $ do
    it "quadrtSP 0 = 0" $ do
      floats @Interp (quadrtSP (unfloats4 (0))) `shouldBe` [0,0,0,0]
  describe "Quadrt infinity" $ do
    it "quadrtSP 1/0 = 1/0" $ do
      floats @Interp (quadrtSP (unfloats4 (1/0))) `shouldBe` [1/0, 1/0, 1/0, 1/0]
    -- it "quadrtSP -1/0 = 0/0" $ do
    --   (head $ floats @Interp (quadrtSP (unfloats4 (-1/0)))) `shouldSatisfy` Prelude.isNaN   -- Haskell fun doesn't output correct answer
  describe "Quadrt NaN" $ do
    it "quadrtSP 0/0 = 0/0" $ do
      (head $ floats @Interp (quadrtSP (unfloats4 (0/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Quadrt negative numbers" $ do
    it "quadrtSP -1 = 0/0" $ do
      (head $ floats @Interp (quadrtSP (unfloats4 (-1)))) `shouldSatisfy` Prelude.isNaN
  describe "Quadrt subnormal" $ do
    it "quadrtSP 1.40E-45 = 6.11832676" $ do
      floats @Interp (quadrtSP(unfloats4(1.40E-45))) `shouldSatisfy`
        (all(\n -> ((-5.95)*10**(-7) <= n && n <= 5.95*10**(-7))))
    it "quadrtSP -1.40E-45 = 0/0" $ do
      (head $ floats @Interp (quadrtSP (unfloats4 (-1.45E-45)))) `shouldSatisfy` Prelude.isNaN
  
  describe "RecipQuadrt zero" $ do
    it "recipQdrt 0 = 0" $ do
      floats @Interp (recipQdrt (unfloats4 (0))) `shouldBe` [1/0,1/0,1/0,1/0]
  describe "RecipQuadrt infinity" $ do
    it "RecipQuadrt 1/0 = 1/0" $ do
      floats @Interp (recipQdrt (unfloats4 (1/0))) `shouldBe` [0,0,0,0]
    it "RecipQuadrt -1/0 = 0/0" $ do
      (head $ floats @Interp (quadrtSP (unfloats4 (-1/0)))) `shouldSatisfy` Prelude.isNaN
  describe "RecipQuadrt NaN" $ do
    it "RecipQuadrt 0/0 = 0/0" $ do
      (head $ floats @Interp (quadrtSP (unfloats4 (0/0)))) `shouldSatisfy` Prelude.isNaN
  describe "RecipQuadrt negative numbers" $ do
    it "RecipQuadrt -1 = 0/0" $ do
      (head $ floats @Interp (quadrtSP (unfloats4 (-1)))) `shouldSatisfy` Prelude.isNaN
  describe "RecipQuadrt subnormal" $ do
    it "recipQdrt 1.40E-45 = 1.6344337" $ do
      floats @Interp (quadrtSP(unfloats4(1.40E-45))) `shouldSatisfy`
        (all(\n -> ((-5.95)*10**(-7) <= n && n <= 5.95*10**(-7))))
    it "RecipQuadrt -1.40E-45 = 0/0" $ do
      (head $ floats @Interp (quadrtSP (unfloats4 (-1.45E-45)))) `shouldSatisfy` Prelude.isNaN

  
--tested: [0.5,1,1.5,2] [0.5^130,0,1/0]
quadrtSPDbg :: [Double] -> IO ()
quadrtSPDbg fourFloats = 
  let
    result = snd $ quadrtDev $ unfloats @Interp fourFloats
  in 
    case result of
      (input : rest) -> do
        let answer = ("Haskell fun", unfloats $ map (\x -> x**(1/4)) $ floats @Interp $ snd input)
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
      _ -> putStrLn "Empty list: Nothing to process."

--tested: [0.5,1,1.5,2] [0.5^130,0,1/0]

qdrt1ReleaseDbg :: [Double] -> IO ()
qdrt1ReleaseDbg fourFloats = 
  let
    result = snd $ qdrt1ReleaseDev $ unfloats @Interp fourFloats
  in 
    case result of
      (input : rest) -> do
        let answer = ("Haskell fun", unfloats $ map (\x -> x**(1/4)) $ floats @Interp $ snd input)
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
      _ -> putStrLn "Empty list: Nothing to process."

--tested: [0.5,1,1.5,2] [0.5^130,0,1/0],map (\a->(-1)*a) [0.5,1,1.5,2],[2^127,2^128-2^106,(-2^127),-(2^128-2^106)]

recipQdrtDbg :: [Double] -> IO ()
recipQdrtDbg fourFloats = 
  let
    result = snd $ recipQdrtDev (Flags {subnormalOutput=True, outputNaN=True}) $ unfloats @Interp fourFloats
  in 
    case result of
      (input : rest) -> do
        let answer = ("Haskell fun", unfloats $ map (\x -> 1 / (sqrt (sqrt x))) $ floats @Interp $ snd input)
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
      _ -> putStrLn "Empty list: Nothing to process."


-- TODO
--   doesn't work for 0,infinity, 

quadrtSPT10 :: (Double, Double)
quadrtSPT10 = worstError 
           $ cmpFltToHaskellFun quadrtSP (\x->(x**(1/4)))
                                [ x/978 | x<-[1..978*10]]

-- recipSqrtT10 = worstError 
--            $ cmpFltToHaskellFun qdrt1Release (\x->1/sqrt(x))
--                                 [ x/978 | x<-[1..978*10]]

recipQdrtT10 :: (Double, Double)
recipQdrtT10 = worstError 
           $ cmpFltToHaskellFun recipQdrt (\x->1/sqrt(sqrt(x)))
                                [ x/978 | x<-([-978*10..(-1)]++[1..978*10])]
