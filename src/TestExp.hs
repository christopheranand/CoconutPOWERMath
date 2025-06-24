module TestExp(test, expT2, expm1T1, expm1D1, exp2Dbg, expDbg) where

import TestUtils 
import Coconut.BaseTypes
import ISA.PowerISA
-- import ISA.PowerInterp
import Exp
import System.Time
import Test.Hspec

-- import Control.Monad
-- import Data.Foldable

-- fexpT scale = map (\x -> (zipWith (\x y -> (x-y)/y) 
--                            (floats @Interp $ fexp expCoeffs24bits $ (unfloats @Interp x)) 
--                            (map exp x) 
--                    )
--             ) 
--   [[i*scale, scale*i*log ((1 - 0.000001)), scale*i*(log ((1+2**(-16)))), scale*i*(log (1.5))]
--   |i<-[-1000..1000]
--   ]


test :: IO ()
test = hspec $ do
  -- Test exp
  describe "expSP Special Values" $ do
    sequence_ $
      map (fmatSpec "expSP" expSP (\x -> exp x) 1 (1/2)) $
        [0,1/0,-1/0,0/0]
  describe "expSP subnormals" $ do
    sequence_ $
      map (fmatSpec "expm1SP" expSP (\x -> exp x) 1 (1/2)) $
        [i | i <- take 50 [1.40e-45, 2.80e-45..]]
  -- Test exp2
  describe "expSP Special Values" $ do
    sequence_ $
      map (fmatSpec "expSP" exp2SP (\x -> 2**x) 1 (1/2)) $
        [0,1/0,-1/0,0/0]
  describe "expSP subnormals" $ do
    sequence_ $
      map (fmatSpec "expm1SP" exp2SP (\x -> 2**x) 1 (1/2)) $
        [i | i <- take 50 [1.40e-45, 2.80e-45..]]

--  Examples
-- run :: IO ()
-- run = hspec $ do
--   describe "Exp tests" $ do
--     it "expSP 0 = 1" $ do
--       floats @Interp (expSP (unfloats4 0)) `shouldBe` [1.0, 1.0, 1.0, 1.0] 
--     it "expSP 0 = 1 within tolerance" $ do
--       floats @Interp (expSP (unfloats4 0))
--         `shouldSatisfy`
--         (all (\n -> 0.1 <= n && n <= 0.9))
--     sequence_ $
--       map (fmatSpec "expm1SP" expm1SP (\x -> exp x - 1) 1 (1/2)) $
--         [i/43 | i<-[-47..47]]++[3/4+1/64,-(3/4+1/64)]


expT2 :: IO ((Double, Double), ClockTime)
expT2 = timeStamp $ worstError $ map (\xs -> (zipWith (\x y -> (x - y) / y)
                                                      (floats @Interp $ expSP $ unfloats @Interp xs)
                                                      (map exp $ floats @Interp $ unfloats @Interp xs)
                                           )
                                   )
                                   [[i * scale, scale * i * log (1 - 0.000001), scale * i * log (1 + 2 ** (-16)), scale * i * log 1.5]
                                    | i <- [-1000 .. 1000]
                                   ]
  where
    scale = 0.001

expm1T1 :: IO ((Double, Double), ClockTime)
expm1T1 = timeStamp $ worstError $
        map (\xs -> (zipWith (\x y -> (x - y) / y) 
                            (floats @Interp $ expm1SP $ unfloats @Interp xs) 
                            (map (\x -> exp x - 1) $ floats @Interp $ unfloats @Interp xs) 
                    )
            ) 
  inputs
  where
    inputs = [[i*scale, scale*i*log (1 - 0.000001), scale*i*(log (1 + 2 ** (-16))), scale*i*(log 1.5)]
              | i <- [-1000..1000]
              ]
    scale = 0.001

expm1D1 :: IO ()
expm1D1 = fmat expm1SP (\ x -> exp x - 1) 1 (1/2) $ [i/43 | i<-[-47..47]]++[3/4+1/64,-(3/4+1/64)]

exp2Dbg :: [Double] -> IO ()
exp2Dbg fourFloats = 
  let
    result = snd $ exp2SPDev $ unfloats @Interp fourFloats
  in case result of
    (input : rest) -> 
      let
        answer = ("Haskell fun", unfloats $ map (\x -> 2**x) $ floats @Interp (unfloats @Interp fourFloats))
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
    _ -> putStrLn "Empty list: Nothing to process."

expDbg :: [Double] -> IO ()
expDbg fourFloats = 
  let
    result = snd $ expSPDev $ unfloats @Interp fourFloats
  in case result of
    (input : rest) -> 
      let
        answer = ("Haskell fun", unfloats $ map (\x -> (exp 1)**x) $ floats @Interp (unfloats @Interp fourFloats))
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
    _ -> putStrLn "Empty list: Nothing to process."
    

-- Biggest value that should return 1 

-- Smallest value which should return > 1

-- Biggest negative value which returns a value of 1

-- Smallest negative value should return < 1

-- Test over the range 

-- Last value which returns a number less than infinity 
-- First value which returns infinity 

-- Smallest negative value that returns a value > 0