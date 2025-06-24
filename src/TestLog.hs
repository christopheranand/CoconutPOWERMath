module TestLog(test,log2Dbg,log2MapDbg,log1pDbg,log1pMapDbg,logeDbg,logeMapDbg,log10Dbg,log10MapDbg,logT10,log2T10,log10T10,log1pT10,logTest,log1pD1,log2D1,log2D2,log2D3,log2D4,log2D5,log2D6,log2Di0,log2Di1,log2Di2,log2Di3,log2Di4,log2Di5,log2Di6,log2Di7,log2Di6b,log2Di7b,logD1) where
import Numeric (showHex)
-- import qualified Data.List as List
import Log
import TestUtils
import Coconut.BaseTypes
import ISA.PowerISA
import Test.Hspec
import System.Time
-- import Control.Monad
-- import Data.Foldable

test :: IO ()
test = hspec $ do
  -- test Log2SP
  describe "Log2 zero" $ do
    it "log2SP 0 = -1/0" $ do
      floats @Interp (log2SP (unfloats4 (0))) `shouldBe` [-1/0, -1/0, -1/0, -1/0]
  describe "Log2 infinity" $ do
    it "log2SP 1/0 = 1/0" $ do
      floats @Interp (log2SP (unfloats4 (1/0))) `shouldBe` [1/0, 1/0, 1/0, 1/0]
    it "log2SP -1/0 = 0/0" $ do
      (head $ floats @Interp (log2SP (unfloats4 (-1/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Log2 NaN" $ do
    it "log2SP 0/0 = 0/0" $ do
      (head $ floats @Interp (log2SP (unfloats4 (0/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Log negative numbers" $ do
    it "log2SP -1 = 0/0" $ do
      (head $ floats @Interp (log2SP (unfloats4 (-1)))) `shouldSatisfy` Prelude.isNaN
  describe "Log2 subnormal" $ do
    it "log2SP 1.40E-45 = -149" $ do
      floats @Interp (log2SP (unfloats4 (1.40E-45))) `shouldBe` [-149.0, -149.0, -149.0, -149.0]
    it "log2SP -1.40E-45 = 0/0" $ do
      (head $ floats @Interp (log2SP (unfloats4 (-1.40E-45)))) `shouldSatisfy` Prelude.isNaN
  
  -- test Log1pSP
  describe "Log1p zero" $ do
    it "log1pSP 0 = 0" $ do
      floats @Interp (log1pSP (unfloats4 (0))) `shouldBe` [0.0, 0.0, 0.0, 0.0]
  describe "Log1p infinity" $ do
    it "log1pSP 1/0 = 1/0" $ do
      floats @Interp (log1pSP (unfloats4 (1/0))) `shouldBe` [1/0, 1/0, 1/0, 1/0]
    it "log1pSP -1/0 = 0/0" $ do
      (head $ floats @Interp (log1pSP (unfloats4 (-1/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Log1p NaN" $ do
    it "log1pSP 0/0 = 0/0" $ do
      (head $ floats @Interp (log1pSP (unfloats4 (0/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Log1p negative numbers" $ do
    it "log1pSP -1 = -1/0" $ do
      floats @Interp (log1pSP (unfloats4 (-1))) `shouldBe` [-1/0, -1/0, -1/0, -1/0]
  describe "Log1p subnormal" $ do
    it "log1pSP 1.40E-45 = 0 within tolerance" $ do
      floats @Interp (log1pSP (unfloats4 (1.40E-45))) `shouldSatisfy`
        (all (\n -> ((-5.95) * 10**(-7)) <= n && n <= 5.95 * 10**(-7)))
    it "log1pSP -1.40E-45 = 0" $ do
      floats @Interp (log1pSP (unfloats4 (-1.40E-45))) `shouldSatisfy`
        (all (\n -> ((-5.95) * 10**(-7)) <= n && n <= 5.95 * 10**(-7)))

  -- test LogeSP
  describe "Loge zero" $ do
    it "logeSP 0 = -1/0" $ do
      floats @Interp (logeSP (unfloats4 (0))) `shouldBe` [-1/0, -1/0, -1/0, -1/0]
  describe "Loge infinity" $ do
    it "logeSP 1/0 = 1/0" $ do
      floats @Interp (logeSP (unfloats4 (1/0))) `shouldBe` [1/0, 1/0, 1/0, 1/0]
    it "logeSP -1/0 = 0/0" $ do
      (head $ floats @Interp (logeSP (unfloats4 (-1/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Loge NaN" $ do
    it "logeSP 0/0 = 0/0" $ do
      (head $ floats @Interp (logeSP (unfloats4 (0/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Loge negative numbers" $ do
    it "logeSP -1 = 0/0" $ do
      (head $ floats @Interp (logeSP (unfloats4 (-1)))) `shouldSatisfy` Prelude.isNaN
  describe "Loge subnormal" $ do
    it "logeSP 1.40E-45 = 0" $ do
      floats @Interp (logeSP (unfloats4 (1.40E-45))) `shouldSatisfy`
        (all (\n -> ((-103.2789306640625) - (1.5351 * 10**(-5))) <= n && n <= ((-103.2789306640625) + (1.5351 * 10**(-5)))))  --129 ulps tolerance
    it "logeSP -1.40E-45 = 0/0" $ do
      (head $ floats @Interp (logeSP (unfloats4 (-1.40E-45)))) `shouldSatisfy` Prelude.isNaN

  -- test Log10SP
  describe "Log10 zero" $ do
    it "log10SP 0 = -1/0" $ do
      floats @Interp (log10SP (unfloats4 (0))) `shouldBe` [-1/0, -1/0, -1/0, -1/0]
  describe "Log10 infinity" $ do
    it "log10SP 1/0 = 1/0" $ do
      floats @Interp (log10SP (unfloats4 (1/0))) `shouldBe` [1/0, 1/0, 1/0, 1/0]
    it "log10SP -1/0 = 0/0" $ do
      (head $ floats @Interp (log10SP (unfloats4 (-1/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Log10 NaN" $ do
    it "log10SP 0/0 = 0/0" $ do
      (head $ floats @Interp (log10SP (unfloats4 (0/0)))) `shouldSatisfy` Prelude.isNaN
  describe "Log10 negative numbers" $ do
    it "log10SP -1 = 0/0" $ do
      (head $ floats @Interp (log10SP (unfloats4 (-1)))) `shouldSatisfy` Prelude.isNaN
  describe "Log10 subnormal" $ do
    it "log10SP 1.40E-45 = 0" $ do
      floats @Interp (log10SP (unfloats4 (1.40E-45))) `shouldSatisfy`
        (all (\n -> ((-44.85347366333008) - (5.95 * 10**(-7))) <= n && n <= ((-44.85347366333008) + (5.95 * 10**(-7)))))
    it "log10SP -1.40E-45 = 0" $ do
      (head $ floats @Interp (log10SP (unfloats4 (-1.40E-45)))) `shouldSatisfy` Prelude.isNaN

log2Dbg :: [Double] -> IO ()
log2Dbg fourFloats =
  case snd $ log2Dev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> (log x) / (log 2)) $ floats @Interp $ snd input)
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])
      

log2MapDbg :: Bool
log2MapDbg = 
  let
    inputList1 = unfloats @Interp $ map (\x -> (log x) / (log 2)) [0,1/0,-1/0,0/0]
    inputList2 = unfloats @Interp $ map (\x -> (log x) / (log 2)) [(-1),4]
    outputList1 = map (flip Numeric.showHex "") $ wrds inputList1
    outputList2 = map (flip Numeric.showHex "") $ wrds inputList2
  in
    outputList1 == ["ff800000","7f800000","7fc00000","7fc00000"] && outputList2 == ["7fc00000","40000000","0","0"] 

-- log2Dbg
-- case              |   result              |   reult (hex)
-- 0                 |    -Inf               |     ff800000
-- inf               |    Inf                |     7f800000
-- -inf              |    NaN                |     7fc00000
-- NaN               |    NaN                |     7fc00000
-- normal (4)        |     2                 |     40000000
-- negative (-1)     |     NaN               |     7fc00000
-- subn (1.40E-45)   | -149.0                |     c3150000

log1pDbg :: [Double] -> IO ()
log1pDbg fourFloats =
  case snd $ log1pDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> log (x + 1)) $ floats @Interp $ snd input)
      in
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])


log1pMapDbg :: Bool
log1pMapDbg = 
  let
    inputList1 = unfloats @Interp $ map (\ x -> log (x + 1)) [0,1/0,-1/0,0/0]
    inputList2 = unfloats @Interp $ map (\ x -> log (x + 1)) [(-1),4]
    outputList1 = map (flip Numeric.showHex "") $ wrds inputList1
    outputList2 = map (flip Numeric.showHex "") $ wrds inputList2
  in
    outputList1 == ["0","7f800000","7fc00000","7fc00000"] && outputList2 == ["ff800000","3fce0210","0","0"] 

-- log1pDbg
-- case              |   result              |   reult (hex)     | expected
-- 0                 |    0                  |     0  
-- inf               |    Inf                |     7f800000
-- -inf              |    NaN                |     7fc00000
-- NaN               |    NaN                |     7fc00000
-- normal (4)        | 1.609438180923462     |     3fce0210 
-- negative (-1)     | 1.609438180923462     |     3fce0210 
-- subn (1.40E-45)   | 1.401298464324817e-45 |     1             | 0

logeDbg :: [Double] -> IO ()
logeDbg fourFloats = 
  case snd $ logeDev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> (log x) / (log (exp 1))) $ floats @Interp (snd input))
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])


logeMapDbg :: Bool
logeMapDbg = 
  let
    inputList1 = unfloats @Interp $ map (\ x -> (log x) / (log (exp 1))) [0,1/0,-1/0,0/0]
    inputList2 = unfloats @Interp $ map (\ x -> (log x) / (log (exp 1))) [(-1),1]
    outputList1 = map (flip Numeric.showHex "") $ wrds inputList1
    outputList2 = map (flip Numeric.showHex "") $ wrds inputList2
  in
    outputList1 == ["ff800000","7f800000","7fc00000","7fc00000"] && outputList2 == ["7fc00000","0","0","0"]

-- logeDbg
-- case              |   result              |   reult (hex)     | expected
-- 0                 |    -Inf               |     ff800000  
-- inf               |    Inf                |     7f800000
-- -inf              |    NaN                |     7fc00000
-- NaN               |    NaN                |     7fc00000
-- normal (1)        |      0                |     0 
-- negative (-1)     |    NaN                |     7fc00000    
-- subn (1.40E-45)   | -103.27894592285156   |     c2ce8ed2      | c2ce8ed0 

log10Dbg :: [Double] -> IO ()
log10Dbg fourFloats = 
  case snd $ log10Dev $ unfloats @Interp fourFloats of
    [] -> putStrLn "Empty list: Nothing to process."
    (input : rest) ->
      let
        answer = ("Haskell fun", unfloats $ map (\x -> (log x) / (log 10)) $ floats @Interp (snd input))
      in 
        putStrLn $ unlines $ map debugOutS (input : rest ++ [answer])


log10MapDbg :: Bool
log10MapDbg = 
  let
    inputList1 = unfloats @Interp $ map (\ x -> (log x) / (log 10)) [0,1/0,-1/0,0/0]
    inputList2 = unfloats @Interp $ map (\ x -> (log x) / (log 10)) [(-1),1]
    outputList1 = map (flip Numeric.showHex "") $ wrds inputList1
    outputList2 = map (flip Numeric.showHex "") $ wrds inputList2
  in
    outputList1 == ["ff800000","7f800000","7fc00000","7fc00000"] && outputList2 == ["7fc00000","0","0","0"] 


-- log10Dbg
-- case              |   result              |   reult (hex)     | expected
-- 0                 |    -Inf               |     ff800000  
-- inf               |    Inf                |     7f800000
-- -inf              |    NaN                |     7fc00000
-- NaN               |    NaN                |     7fc00000
-- normal (4)        |  0.602060079574585    |     3f1a209c      | 3f1a209b
-- subn (1.40E-45)   | -44.85347366333008    |     c23369f5      | c23369f4


logT10 :: IO ((Double, Double), ClockTime)
logT10 = timeStamp $ worstError $ cmpFltToHaskellFun logeSP (log) [ x/978 | x<-[1..978*10]]

log2T10 :: IO ((Double, Double), ClockTime)
log2T10 = timeStamp $ worstError 
           $ cmpFltToHaskellFun log2SP (\ x -> log x / log 2)
                                [ x/978 | x<-[1..978*10]]

log10T10 :: IO ((Double, Double), ClockTime)
log10T10 = timeStamp $ worstError 
           $ cmpFltToHaskellFun log10SP (\ x -> log x / log 10)
                                [ x/978 | x<-([1..(-1)]++[1..978*10])]

log1pT10 :: IO ((Double, Double), ClockTime)
log1pT10 = timeStamp $ worstError 
           $ cmpFltToHaskellFun log1pSP (\ x -> log (x+1) )
                                [ x/978-1  | x<-([1..3*977])]

logTest :: IO ()
logTest = fmat logeSP (\ x -> log x) 10 (1/2) [ 1+x/1024 | x<-([-82..283])]
 
log1pD1 :: IO ()
log1pD1 = fmat log1pSP (\ x -> log (x + 1)) 1 1 [ 1.5*x/978 -1/2 | x<-([1..(-1)]++[1..977])]

log2D1 :: IO ()
log2D1 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1+x/1024 | x<-([-82..283])]
log2D2 :: IO ()
log2D2 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1+x/2048 | x<-([-22..0])]
log2D3 :: IO ()
log2D3 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1+x/2048 | x<-([-22..0])]
log2D4 :: IO ()
log2D4 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1+x/811 | x<-([0..810])]
log2D5 :: IO ()
log2D5 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 8.705e-01+x/811000 | x<-([-810..810])]
log2D6 :: IO ()
log2D6 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1+8.705e-01+x/811000 | x<-([-810..810])]
log2Di0 :: IO ()
log2Di0 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1+x/333/8 | x<-([0..332])]
log2Di1 :: IO ()
log2Di1 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1.125+x/333/8 | x<-([0..332])]
log2Di2 :: IO ()
log2Di2 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1.25+x/333/8 | x<-([0..332])]
log2Di3 :: IO ()
log2Di3 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1.375+x/333/8 | x<-([0..332])]
log2Di4 :: IO ()
log2Di4 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1.5+x/333/8 | x<-([0..332])]
log2Di5 :: IO ()
log2Di5 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1.625+x/333/8 | x<-([0..332])]
log2Di6 :: IO ()
log2Di6 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1.75+x/333/8 | x<-([0..332])]
log2Di7 :: IO ()
log2Di7 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1.875+x/333/8 | x<-([0..332])]
log2Di6b :: IO ()
log2Di6b = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 0.75+x/333/8 | x<-([0..332])]
log2Di7b :: IO ()
log2Di7b = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 0.875+x/333/8 | x<-([0..332])]
logD1 :: IO ()
logD1 = fmat log2SP (\ x -> log (x) / log 2) 10 (1/2) [ 1+x/1024 | x<-([-82..283])]