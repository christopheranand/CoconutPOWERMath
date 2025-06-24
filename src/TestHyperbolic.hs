module TestHyperbolic(test,randvf,p1,p2,p3,p4,p5,sinhD1,sinhD2,sinhD3,sinhD4,sinhD5,coshD1,coshD2,coshD3,coshD4,coshD5,fcoshAlt1,fcoshAlt2,fcoshAlt3,fcoshAlt4,fcoshAlt5,ftanh1,ftanh2,ftanh1Dev,tanhDebug,sinhDbg,coshDbg,fcoshAltDbg) where

-- import MathUtils
import TestUtils
import Coconut.BaseTypes
import ISA.PowerISA

-- import Data.List
import Hyperbolic
import System.Random

import Test.Hspec

-- import Control.Monad
-- import Data.Foldable



test :: IO ()
test = hspec $ do
  -- Test sinh
  describe "sinhSP Special Values" $ do
    sequence_ $
      map (fmatSpec "sinhSP" sinhSP (\x -> Prelude.sinh x) 1 (1/2)) $
        [0,1/0,-1/0,0/0]
  describe "sinhSP subnormals" $ do
    sequence_ $
      map (fmatSpec "sinhSP" sinhSP (\x -> Prelude.sinh x) 1 (1/2)) $
        [i | i <- take 50 [1.40e-45, 2.80e-45..]]
  -- Test cosh
  describe "coshSP Special Values" $ do
    sequence_ $
      map (fmatSpec "coshSP" coshSP (\x -> Prelude.cosh x) 1 (1/2)) $
        [0,1/0,-1/0,0/0]
  describe "coshSP subnormals" $ do
    sequence_ $
      map (fmatSpec "coshSP" coshSP (\x -> Prelude.cosh x) 1 (1/2)) $
        [i | i <- take 50 [1.40e-45, 2.80e-45..]]
  -- Test tan
  describe "tanhSP Special Values" $ do
    sequence_ $
      map (fmatSpec "tanhSP" ftanh1 (\x -> Prelude.tanh x) 1 (1/2)) $
        [0,1/0,-1/0,0/0]
  describe "tanhSP subnormals" $ do
    sequence_ $
      map (fmatSpec "tanhSP" ftanh1 (\x -> Prelude.tanh x) 1 (1/2)) $
        [i | i <- take 50 [1.40e-45, 2.80e-45..]]


-- Generate random vectors with float values in a range:
randvf :: Int -> (Double, Double) -> [Interp VR]
randvf seed (low,high) = randvf' randfs
  where
    randfs = randomRs (low,high) (mkStdGen seed)
    randvf' (f1:f2:f3:f4:g4) = (unfloats @Interp [f1,f2,f3,f4]) : (randvf' g4)
    randvf' _ = error "randvf:: can't occur"

p1 :: IO () 
p1 = TestUtils.fmat ftanh1 (\ x -> tanh x) 0 (1**(-30)) [i/10 | i <- [1..10]]
p2 :: IO ()
p2 = TestUtils.fmat ftanh2 (\ x -> tanh x) 0 0 [ i/1000 | i <- [1..1000]]
p3 :: IO ()
p3 = TestUtils.fmat ftanh1 (\ x -> tanh x) 0 0 [0.5+ i/10 | i <- [1..10]]
p4 :: IO ()
p4 = TestUtils.fmat ftanh2 (\ x -> tanh x) 0 0 [2**i | i <- [1..20]]
p5 :: IO ()
p5 = TestUtils.fmat ftanh1 (\ x -> tanh x) 0 (1**(-24)) [i/123 | i <- [1..1000]]

sinhD1 :: IO ()
sinhD1 = TestUtils.fmat sinhSP (\ x -> sinh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ i/10 | i <- [1..10]]
sinhD2 :: IO ()
sinhD2 = TestUtils.fmat sinhSP (\ x -> sinh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $  i/79 | i <- [1..1000]] --Doesn't work for values bigger than 2
sinhD3 :: IO ()
sinhD3 = TestUtils.fmat sinhSP (\ x -> sinh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ 0.5+ i/10 | i <- [1..10]]
sinhD4 :: IO ()
sinhD4 = TestUtils.fmat sinhSP (\ x -> sinh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ 2**i | i <- [1..20]]
sinhD5 :: IO ()
sinhD5 = TestUtils.fmat sinhSP (\ x -> sinh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ i/123 | i <- [1..1000]]

coshD1 :: IO ()
coshD1 = TestUtils.fmat coshSP (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ i/10 | i <- [1..10]]
coshD2 :: IO ()
coshD2 = TestUtils.fmat coshSP (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $  i/79 | i <- [1..1000]] 
coshD3 :: IO ()
coshD3 = TestUtils.fmat coshSP (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ 0.5+ i/10 | i <- [1..10]]
coshD4 :: IO ()
coshD4 = TestUtils.fmat coshSP (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ 2**i | i <- [1..20]]
coshD5 :: IO ()
coshD5 = TestUtils.fmat coshSP (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ i/123 | i <- [1..1000]]

fcoshAlt1 :: IO ()
fcoshAlt1 = TestUtils.fmat fcoshAlt (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ i/10 | i <- [1..10]]
fcoshAlt2 :: IO ()
fcoshAlt2 = TestUtils.fmat fcoshAlt (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $  i/79 | i <- [1..1000]] 
fcoshAlt3 :: IO ()
fcoshAlt3 = TestUtils.fmat fcoshAlt (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ 0.5+ i/10 | i <- [1..10]]
fcoshAlt4 :: IO ()
fcoshAlt4 = TestUtils.fmat fcoshAlt (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ 2**i | i <- [1..20]]
fcoshAlt5 :: IO ()
fcoshAlt5 = TestUtils.fmat fcoshAlt (\ x -> cosh x) 0 (1/2) [(head . floats . unfloats4 @Interp) $ i/123 | i <- [1..1000]]

ftanh1 :: (PowerISA repr) => repr VR -> repr VR
ftanh1 x = fst $ tanhSP (x, unfloats4 0)
ftanh2 :: (PowerISA repr) => repr VR -> repr VR
ftanh2 x = snd $ tanhSP (unfloats4 0,x)

ftanh1Dev :: (PowerISA repr) => repr VR -> (repr VR, ((repr VR, repr VR), [(String, repr VR)]))
ftanh1Dev x = (fst $ tanhSP (x, unfloats4 0), tanhDbg (x, unfloats4 0))

tanhDebug :: [Double] -> IO ()
tanhDebug fourFloats =
  case snd $ snd $ ftanh1Dev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> tanh x) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])


sinhDbg :: [Double] -> IO ()
sinhDbg fourFloats =
  case snd $ sinhSPDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> sinh x) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
        
  
coshDbg :: [Double] -> IO ()
coshDbg fourFloats =
  case snd $ coshSPDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> cosh x) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
        

fcoshAltDbg :: [Double] -> IO ()
fcoshAltDbg fourFloats =
  case snd $ fcoshAltDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> cosh x) $ floats @Interp (unfloats @Interp fourFloats))
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])

