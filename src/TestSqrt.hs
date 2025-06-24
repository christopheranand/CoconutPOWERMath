module TestSqrt(sqrtAbsDbg,recipDbg,rsqrtDbg,recipSqrtAbsDbg,sqrtAbsT10,sqrtSPT10,recipSqrtAbsT10,recipSqrtT10,frecipT10) where

-- import qualified Data.List as List
import Sqrt

import Coconut.BaseTypes
import ISA.PowerISA
-- import ISA.PowerInterp
import TestUtils

sqrtAbsDbg :: [Double] -> IO ()
sqrtAbsDbg fourFloats = 
  let
    result = snd $ sqrtAbsDev $ unfloats @Interp fourFloats
  in 
    case result of
      (input : rest) -> do
        let answer = ("Haskell fun", unfloats $ map (\x -> sqrt $ abs x) $ floats @Interp $ snd input)
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
      _ -> putStrLn "Empty list: Nothing to process."


--tested: [0.5,1,1.5,2] [0.5^130,0,1/0]
recipDbg :: [Double] -> IO ()
recipDbg fourFloats = 
  let
    result = snd $ recipDev $ unfloats @Interp fourFloats
  in 
    case result of
      (input : rest) -> do
        let answer = ("Haskell fun", unfloats $ map (\x -> 1 / x) $ floats @Interp $ snd input)
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
      _ -> putStrLn "Empty list: Nothing to process."


--tested: [0.5,1,1.5,2] [0.5^130,0,1/0]
rsqrtDbg :: [Double] -> IO ()
rsqrtDbg fourFloats = putStrLn $ unlines $ map debugOutS (snd $ rsqrtDev $ unfloats  @Interp fourFloats)

--tested: [0.5,1,1.5,2] [0.5^130,0,1/0],map (\a->(-1)*a) [0.5,1,1.5,2],[2^127,2^128-2^106,(-2^127),-(2^128-2^106)]
recipSqrtAbsDbg :: [Double] -> IO ()
recipSqrtAbsDbg fourFloats = 
  let
    result = snd $ recipSqrtAbsDev $ unfloats @Interp fourFloats
  in 
    case result of
      (input : rest) -> do
        let answer = ("Haskell fun", unfloats $ map (\x -> 1 / (sqrt (abs x))) $ floats @Interp $ snd input)
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
      _ -> putStrLn "Empty list: Nothing to process."


-- TODO
--   doesn't work for 0,infinity, 

--lhs:(-22.165487001465205,2.125805586688935e-7)
--actual:(-22.785746408970034,1.3829512057776437e-7)
sqrtAbsT10 :: (Double, Double)
sqrtAbsT10 = worstError 
   $ cmpFltToHaskellFun sqrtAbs (sqrt . abs) [ x/978 | x<-[-978*10..978*10]]

--lhs:(-22.165487001465205,2.125805586688935e-7)
--actual:(-22.785746408970034,1.3829512057776437e-7)
sqrtSPT10 :: (Double, Double)
sqrtSPT10 = worstError 
           $ cmpFltToHaskellFun sqrtSP (sqrt)
                                [ x/978 | x<-[1..978*10]]

--lhs: (-22.927848083057214,1.253227715807695e-7)
--actual: (-23.31142849960002,9.606402513158541e-8)    
recipSqrtAbsT10 :: (Double, Double)                            
recipSqrtAbsT10 = worstError 
           $ cmpFltToHaskellFun recipSqrtAbs (\x->1/sqrt(abs x))
                                [ x/978 | x<-([-978*10..(-1)]++[1..978*10])]
--lhs: (-22.927848083057214,1.253227715807695e-7)
--actual: (-23.31142849960002,9.606402513158541e-8)
recipSqrtT10 :: (Double, Double)
recipSqrtT10 = worstError 
           $ cmpFltToHaskellFun rsqrtSP (\x->1/sqrt(x))
                                [ x/978 | x<-[1..978*10]]

--actual: (-23.40633583375457,8.994785796912838e-8)
frecipT10 :: (Double, Double)
frecipT10 = worstError 
           $ cmpFltToHaskellFun recipSP (\x->1/x)
                                [ x/978 | x<-([-978*10..(-1)]++[1..978*10])]
