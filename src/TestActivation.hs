module TestActivation(geluOneDbg, geluTwoDbg, geluThreeDbg, sigmoidDbg, siluDbg, softplusDbg, sig, siluhelp, softpls) where

-- import qualified Data.List as List
-- import MathUtils
import TestUtils
import Coconut.BaseTypes
import ISA.PowerISA

import Activation

geluOneDbg :: [Double] -> IO ()
geluOneDbg fourFloats =
  case snd $ geluOneDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> 0.5 * x * 2*(exp 1)**(2*x) / (1 + (exp 1)**(2*x))) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])


-- Columns are: sign, exponent, x (dec), x (hex), test (dec, hex), true (dec, hex), ulp err, ulp err (dif of integers)

-- $ cat vsgelu.acc
-- &x    =140736433085072 mod16=0
-- &ytest=140736433085920 mod16=0
-- -1   127 -1.70e+38 FF000000  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   126 -1.66e+38 FEF9C938  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   125 -4.74e+37 FE0EA7BB  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   124 -3.35e+37 FDC9BB28  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   123 -1.31e+37 FD1E1D84  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   122 -7.81e+36 FCBC21AF  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   121 -3.47e+36 FC270447  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   120 -1.71e+36 FBA46092  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   119 -7.18e+35 FB0A3E01  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   118 -4.57e+35 FAB02256  0.00e+00 00000000 -0.00e+00 80000000  0.00e+00 -2.15e+09
-- -1   117 -1.86e+35 FA0F47C5  0.00e+00 00000000 -0.00e+00 8000


geluTwoDbg :: [Double] -> IO ()
geluTwoDbg eightFloats =
  case snd $ geluTwoDev $ (unfloats $ take 4 eightFloats, unfloats $ drop 4 eightFloats) of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> 0.5*x*(1+tanh(sqrt(2/pi)*(x+0.044715*x^(3 :: Integer))))) $ floats @Interp (unfloats @Interp eightFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])



geluThreeDbg :: [Double] -> IO ()
geluThreeDbg fourFloats =
  case snd $ geluThreeDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> x*sig(1.702*x)) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
        

sigmoidDbg :: [Double] -> IO ()
sigmoidDbg fourFloats =
  case snd $ sigmoidDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (sig) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
        

siluDbg :: [Double] -> IO ()
siluDbg fourFloats =
  case snd $ siluDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (siluhelp) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
        

softplusDbg :: [Double] -> IO ()
softplusDbg fourFloats =
  case snd $ softplusDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (softpls) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
        

sig :: Double -> Double
sig x = 1 / (1 + exp(-x))
siluhelp :: Double -> Double
siluhelp x = x * sig x
softpls :: Double -> Double
softpls x = log(1 + exp(x))
