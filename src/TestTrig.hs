-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module TestTrig(test,p1,p2,p3,p4,p5,p6,sinD1,sinD2,sinD3,tanD1,tanD2,tanD3,sincosD1,sincosD2,sincosD3,cossc,cosscf,sinDbg,tanDbg) where

import TestUtils
import Coconut.BaseTypes
import ISA.PowerISA
import Trig
import GHC.Word

import Test.Hspec

test :: IO ()
test = hspec $ do
  -- Test sin
  describe "sinSP Special Values" $ do
    sequence_ $
      map (fmatSpec "sinSP" sinSP (\x -> Prelude.sin x) 1 (1/2)) $
        [0,1/0,-1/0,0/0]
  describe "sinSP subnormals" $ do
    sequence_ $
      map (fmatSpec "sinSP" sinSP (\x -> Prelude.sin x) 1 (1/2)) $
        [i | i <- take 50 [1.40e-45, 2.80e-45..]]
  -- Test cos
  describe "cosSP Special Values" $ do
    sequence_ $
      map (fmatSpec "cosSP" cosSP (\x -> Prelude.cos x) 1 (1/2)) $
        [0,1/0,-1/0,0/0]
  describe "cosSP subnormals" $ do
    sequence_ $
      map (fmatSpec "cosSP" cosSP (\x -> Prelude.cos x) 1 (1/2)) $
        [i | i <- take 50 [1.40e-45, 2.80e-45..]]
  -- Test tan
  describe "tanSP Special Values" $ do
    sequence_ $
      map (fmatSpec "tanSP" tanSP (\x -> Prelude.tan x) 1 (1/2)) $
        [0,1/0,-1/0,0/0]
  describe "tanSP subnormals" $ do
    sequence_ $
      map (fmatSpec "tanSP" tanSP (\x -> Prelude.tan x) 1 (3/2)) $
        [i | i <- take 50 [1.40e-45, 2.80e-45..]]


p1 :: IO ()
p1 = TestUtils.fmat Trig.cosSP cos 0 (1**(-24)) [i/10 | i <- [1..10]]
p2 :: IO ()
p2 = TestUtils.fmat Trig.cosSP cos 0 (1**(-24)) [pi + i/10 | i <- [1..10]]
p3 :: IO ()
p3 = TestUtils.fmat Trig.cosSP cos 0 0 [ i/1000 | i <- [1..1000]]
p4 :: IO ()
p4 = TestUtils.fmat Trig.cosSP cos 0 0 [0.5+ i/10 | i <- [1..10]]
p5 :: IO ()
p5 = TestUtils.fmat Trig.cosSP cos 0 0 [2**i | i <- [1..20]]
p6 :: IO ()
p6 = TestUtils.fmat Trig.cosSP cos (1**(-25)) (1**(-25)) [i/123 | i <- [1..1000]]

sinD1 :: IO ()
sinD1 = TestUtils.fmat sinSP sin 10 (1/2) [i/13*pi/4 | i <- [0..53]]
sinD2:: IO ()
sinD2 = TestUtils.fmat sinSP sin 10 (1/2) [i*pi | i <- [-200..200]]
sinD3 :: IO ()
sinD3 = TestUtils.fmat Trig.sinSP sin 10 (1/2) [i*2**(-25) | i <- [-20..20]]

tanD1 :: IO ()
tanD1 = TestUtils.fmat Trig.tanSP Prelude.tan 10 (1/2) [i/13*pi/4 | i <- [0..53]]
tanD2:: IO ()
tanD2 = TestUtils.fmat Trig.tanSP Prelude.tan 10 (1/2) [i*pi | i <- [-200..200]]
tanD3 :: IO ()
tanD3 = TestUtils.fmat Trig.tanSP Prelude.tan 10 (1/2) [i*2**(-25) | i <- [-20..20]]

sincosD1 :: IO ()
sincosD1 = TestUtils.fmat (fst . Trig.sincosSP) sin 1 (10) [i/43*pi/4 | i <- [0..173]]
sincosD2 :: IO ()
sincosD2 = TestUtils.fmat (snd . Trig.sincosSP) cos 1 (0) [i/43*pi/4 | i <- [0..173]]
sincosD3 :: IO ()
sincosD3 = TestUtils.fmat (\ x -> xvmaddmsp (fst $ Trig.sincosSP x) (fst $ Trig.sincosSP x) (xvmulsp (snd $ Trig.sincosSP x) (snd $ Trig.sincosSP x))) (\ _ -> 1) 10 (1/2) [i/13*pi/4 | i <- [0..253]]

cossc :: GHC.Word.Word32 -> [Double]
cossc x = floats @Interp (snd $ Trig.sincosSP $ unwrds4 x)
cosscf :: Double -> [Double]
cosscf x = floats @Interp (snd $ Trig.sincosSP $ unfloats4 x)

sinDbg :: [Double] -> IO ()
sinDbg fourFloats =
  let
    result = snd $ sinSPDev (unfloats @Interp fourFloats)
  in case result of
    (input : rest) -> 
      let
        answer = ("Haskell fun", unfloats $ map (\x -> sin x) $ floats @Interp (unfloats @Interp fourFloats))
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
    _ -> putStrLn "Empty list: Nothing to process."



tanDbg :: [Double] -> IO ()
tanDbg fourFloats =
  let
    result = snd $ tanSPDev $ unfloats @Interp fourFloats
  in case result of
    (input : rest) -> 
      let
        answer = ("Haskell fun", unfloats $ map (\x -> Prelude.tan x) $ floats @Interp (unfloats @Interp fourFloats))
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
    _ -> putStrLn "Empty list: Nothing to process."
    