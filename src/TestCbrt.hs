module TestCbrt(rcbrtDbg, cbrtDbg) where
import Cbrt
import TestUtils
--import PrelExts
import ISA.PowerISA
-- import ISA.PowerInterp
import Coconut.BaseTypes

rcbrtDbg :: [Double] -> IO ()
rcbrtDbg fourFloats =
  case snd $ rcbrtDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> x ** (-1 / 3)) $ floats @Interp $ snd input)
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])


cbrtDbg :: [Double] -> IO ()
cbrtDbg fourFloats =
  case snd $ cbrtDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> x ** (1 / 3)) $ floats @Interp $ snd input)
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
        


{-t1 = cmpFltToHaskellFun cbrtSP (\x -> x**(1/3)) [1..10]

p1 = TestUtils.fmat Cbrt.cbrtSP (\ x -> x**(1/3)) 0 (2**(-24)) [i/10 | i <- [1..10]]
p2 = TestUtils.fmat Cbrt.cbrtSP (\ x -> x**(1/3)) 0 0 [ i/1000 | i <- [1..1000]]
p3 = TestUtils.fmat Cbrt.cbrtSP (\ x -> x**(1/3)) 0 0 [0.5+ i/10 | i <- [1..10]]
p4 = TestUtils.fmat Cbrt.cbrtSP (\ x -> x**(1/3)) 0 0 [2**i | i <- [1..20]]
p5 = TestUtils.fmat Cbrt.cbrtSP (\ x -> x**(1/3)) (1**(-24)) (1**(-24)) [i/123 | i <- [1..1000]]


rems lInts = map (flip divMod 64) $ bytes $ Cbrt.cbrtSP (unfloats lInts::Val)

-- rem1 lInts = wrds $ xvcmpgtsp (Cbrt.cbrtSP (unfloats lInts::Val)) (2^6)
-- rem2 lInts = wrds $ xvcmpgtsp (Cbrt.cbrtSP (unfloats lInts::Val)) (2^7)

pexp3 llInts = sequence $ map (putStrLn.show) 
        [(bytes $ Cbrt.cbrtSP (unfloats lInts :: Val),lInts)
        | lInts <- llInts]


-- pexp3' llInts = sequence $ map (putStrLn.show) 
--         [(floats $ signCbrtExp $ Cbrt.cbrtSP (unfloats lInts :: Val),map (\x->x**(1/3)) lInts)
--         | lInts <- llInts]
--   where
--     signCbrtExp expDiv3shift16 = xxsel (unfloats4 1) (vrlq expDiv3shift16 7) 
--                 $ unwrds4 $ 2^31-2^23


x keyPos = map (bytes . (key8Word keyPos) . unbytes) $ chunks 16 [0..255]-}
