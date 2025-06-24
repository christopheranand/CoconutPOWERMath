{-
\section{Helper functions for testing SPU functions}
LMD: I only put the minimal required functions that are used in the MASS
code, to avoid as many external dependencies as possible
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE NoMonomorphismRestriction#-}
{-# LANGUAGE TypeApplications #-}

module TestUtils(padTo,padLeftTo,debugOut,dbxOut,debugOut1S,debugQ,allOrOneM,allOrOne,headX,ftest,fabserr,fabserrWrapper,fabsrelerr,frelerr,frelerrWrapper,fabs,ferr,fmat,fmatSpec,dmat,dmat2,fillRight,dabserr,dabsrelerr,drelerr,dabs,derr,dtest,fp32,fp64,randTest64,randTest32,singleTest,randTest,ulp64,ulp32,eps32,eps64,eps,ulp,lst2Q,worstError,timeStamp,testf2,cmpFltToHaskellFun,cmpFltToHaskellFun2,permuteByFour,debugOutS) where

import Coconut.BaseTypes
import Numeric
import ISA.PowerISA
import ISA.PowerInterp
import Text.Printf
import Data.List(zipWith4,maximumBy)
import Data.Function (on)
import System.Random
import System.Time
import Control.Monad
-- import Control.Parallel
import Control.Parallel.Strategies
import Coconut.Utils.ArbFloat (dbl2Word64)
import Test.Hspec
-- import Control.Monad.IO.Class

-- type Val = Interp VR

{-
%}}}

%{{{ padTo, padTruncateTo, padLeftTo
The following padding functions will \emph{not} truncate |xs|
if it is longer than |k|,
but it will pad it to length |k| if it is shorter:

-}
padTo :: Int -> a -> [a] -> [a]
padTo k x xs = xs ++ replicate (k - length xs) x

{-

Padding on the left instead:
-}
padLeftTo :: Int -> a -> [a] -> [a]
padLeftTo k x xs = replicate (k - length xs) x ++ xs

{-

Formatting debug print for functions returning (result,debug).
-}

-- DOUBLE Precision
debugOut, dbxOut :: (String, Interp VR) -> String
debugOut (name', v1) = (take 40 $ name' ++ "                                         ")
  ++ " | " ++ (concatMap ((take 25) . (++"                         ") . show) $ doubles v1)
  ++ (concat $ zipWith (++) ["| ",", ",", ",", ","| ",", "] $ map (reverse . (take 16) . reverse . ("                     " ++ ) . (flip showHex "")) $ dwrds v1)

dbxOut (name', v1) = (take 30 $ name' ++ "                                         ")
  ++ " | " ++ (concatMap ((take 25) . (++"                         ") . show) $ doubles v1)
  ++ (concat $ zipWith (++) ["| (0x",",0x",",0x",",0x"] $ map (reverse . (take 8) . reverse . ("0000000000" ++ ) . (flip showHex "")) $ wrds v1)
  ++ ")"


-- debugOut1 :: (String, Interp VR) -> String
-- debugOut1 (name,v1) = (take 18 $ name ++ "                        ")
--   ++ " | " ++ (concatMap ((take 25) . (++"                         ") . show) $ take 1 $ doubles v1)
--   ++ (concat $ zipWith (++) ["| ","| "] $ map (reverse . (take 16) . reverse . ("                     " ++ ) . (flip showHex "")) $ take 1 $ dwrds v1)

-- SINGLE Precision
debugOutS :: (String, Interp VR) -> String
debugOutS (name', v1) = (take 25 $ name' ++ "                                         ")
  ++ (concatMap ((take 22) . (++"                         ") . show) $ floats v1)
  ++ (concat $ zipWith (++) ["| ",", ",", ",", ","| ",", "] $ map (reverse . (take 8) . reverse . ("                     " ++ ) . (flip showHex "")) $ wrds v1)


-- dbxOutS:: (String, Interp VR) -> String
-- dbxOutS (name,v1) = (take 30 $ name ++ "                                         ")
--   ++ " | " ++ (concatMap ((take 25) . (++"                         ") . show) $ floats v1)
--   ++ (concat $ zipWith (++) ["| (0x",",0x",",0x",",0x"] $ map (reverse . (take 8) . reverse . ("0000000000" ++ ) . (flip showHex "")) $ wrds v1)
--   ++ ")"

debugOut1S :: (String, Interp VR) -> String
debugOut1S (name', v1) = (take 18 $ name' ++ "                        ")
  ++ " | " ++ (concatMap ((take 12) . (++"                         ") . show) $ take 1 $ floats v1)
  ++ (concat $ zipWith (++) ["| ","| "] $ map (reverse . (take 8) . reverse . ("                     " ++ ) . (flip showHex "")) $ take 1 $ wrds v1)


debugQ :: Bool
debugQ = True

allOrOneM :: Foldable t => t ([Char], (b, b, b, b)) -> [([Char], b)]
allOrOneM = concatMap allOrOne

allOrOne :: ([Char], (b, b, b, b)) -> [([Char], b)]
allOrOne (name', (v0, v1, v2, v3)) = if debugQ then zipWith (\i v -> (name' ++ show (i :: Integer), v)) [0..] [v0, v1, v2, v3]
                                              else [(name', v0)]



headX :: Show a1 => [Char] -> a1 -> [a2] -> a2
headX _ _ (x:_) = x
headX f l _ = error ("head [] at " ++ f ++ ":" ++ show l)
                                              

ftest :: (Interp VR -> Interp VR) -> Double -> Double
ftest f x = case fxs of
    (fx1 : _) -> if allEqual fxs || all isNaN fxs
                 then fx1
                 else error $ "input " ++ show x ++ " gave output " ++ show fxs
                              ++ " (" ++ showHex (fromInteger (integer xs) :: Int) "" ++ ")"
    [] -> error "Empty result list from floats."
  where
    fxs = floats $ f xs
    xs = unfloats4 x

    allEqual :: Eq a => [a] -> Bool
    allEqual [] = True
    allEqual (y:ys) = all (== y) ys





fabserr :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double
fabserr f g x = abs (ftest f x - g x)
fabserrWrapper :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double
fabserrWrapper f g x =
  if (abs x == 1/0) 
      then 0
    else if (isNaN x) 
      then 0
    else fabserr f g x

fabsrelerr :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double
fabsrelerr f g x = abs (fabserr f g x / g x)

frelerr :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double
frelerr f g x = (ftest f x - g x) / abs (g x)

frelerrWrapper :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double
frelerrWrapper f g x = 
    if (abs x == 1/0) 
      then 0
    else if (isNaN x || isNaN (g x) || 0 == (g x)) 
      then 0
    else frelerr f g x

fabs :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> [Double] -> [(Double, Double)]
fabs f g e x = filter ((> e) . snd) (zip x (map (fabserr f g) x))

ferr :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> [Double] -> [(Double, Double)]
ferr f g e x = filter ((> e) . snd) (zip x (map (fabsrelerr f g) x))

fmat :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double -> [Double] -> IO ()
fmat f g ea er x = do putStrLn header
                      putStrLn (replicate 110 '-')
                      _ <- mapM putStrLn rows  -- Use `_ <-` to suppress the warning
                      putStrLn []
                      putStrLn ("Values failed: " ++ show (length rows :: Int))
                      putStrLn ("Values tested: " ++ show (length x :: Int))
                      putStrLn []
                      putStrLn ("Percent goodness: " ++ show (100 * (1 - fromIntegral (length rows) / fromIntegral (length x)) :: Double) ++ "%")
  where header = fillRight 22 "Input Argument" 
              ++ fillRight 22 "Absolute Error" 
              ++ fillRight 22 "Relative Error"
              ++ fillRight 22 "Haskell"
              ++ fillRight 22 "SPU"
        rows  = map showRow $ filter isBad 
              $ map (\ y -> (y, (fabserr f g y) * 2^(23 :: Integer), (frelerr f g y) * 2^(23 :: Integer),g y,ftest f y)) 
              $ map (head . floats @Interp . unfloats4) x
        isBad (_,a,r,_,_) = a >= ea || abs(r) >= er
        showRow (y,a,r,h,s) = fillRight 22 (show y) 
                       ++ fillRight 22 (toShow a ea) 
                       ++ fillRight 22 (toShow r er)
                       ++ fillRight 22 (show h)
                       ++ fillRight 22 (show s)
        toShow z ez | abs(z) >= ez = show z
                    | otherwise = []


fmatSpec :: String -> (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double -> Double -> SpecWith (Arg Expectation)
fmatSpec funcName f g ea er x =
  let
    testValFloat = head . floats @Interp . unfloats4 $ x
    absErrVal = fabserrWrapper f g testValFloat
    relErrVal = frelerrWrapper f g testValFloat
    haskVal = g x
    testVal = ftest f x
    testHeader = printf "%s: Input = %f   HaskResult = %f   PowerResult = %f" funcName x haskVal testVal
    absErrorHeader = printf "Test Abs error = %.9f" absErrVal
    relErrorHeader = printf "Test Rel error = %.9f" relErrVal
  in 
    describe testHeader $ do
      it absErrorHeader $ do -- ("test absErr for " ++ funcName ++ "(" ++ show x ++ ")") $ do
        absErrVal `shouldSatisfy` (< ea)
      it relErrorHeader $ do -- ("test relErr for " ++ funcName ++ "(" ++ show x ++ ")") $ do
        abs(relErrVal) `shouldSatisfy` (< er)

dmat :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double -> [Double] -> IO ()
dmat f g ea er x = do putStrLn header
                      putStrLn (replicate 110 '-')
                      _ <- mapM putStrLn rows  -- Use `_ <-` to suppress the warning
                      putStrLn []
                      putStrLn ("Values failed: " ++ show (length rows))
                      putStrLn ("Values tested: " ++ show (length x))
                      putStrLn []
                      putStrLn ("Percent goodness: " ++ show (100 * (1 - fromIntegral (length rows) / fromIntegral (length x)) :: Double)  ++ "%")
  where header = fillRight 25 "Input Argument"
              ++ fillRight 25 "Absolute Error"
              ++ fillRight 25 "Relative Error"
              ++ fillRight 25 "Haskell"
              ++ fillRight 25 "SPU"
        rows  = map showRow $ filter isBad
              $ map (\ y -> (y, (dabserr f g y) * 2^(53 :: Integer), (drelerr f g y) * 2^(53 :: Integer),g y,dtest f y))
              $ map ((headX "testUtils" (72 :: Integer)) . (doubles @Interp) . undoubles2) x
        isBad (_,a,r,_,_) = a >= ea || abs(r) >= er
        showRow (y,a,r,h,s) = fillRight 22 (show y)
                        ++ fillRight 25 (toShow a ea)
                        ++ fillRight 25 (toShow r er)
                        ++ fillRight 25 (show h)
                        ++ fillRight 25 (show s)
        toShow z ez | abs(z) >= ez = show z
                    | otherwise = []
        


dmat2 :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double -> [Double] -> IO ()
dmat2 f g aemin remin xs = do
    putStrLn $ printf "%9s | %9s | %9s | %9s | %9s | %16s | %16s | %16s"
        "abserr" "relerr" "xtest" "ytest" "ytrue" "xtest" "ytest" "ytrue"
    let results = filter isBad $ parMap rdeepseq (singleTest f g fp64) xs
    mapM_ (putStrLn . showRow) results
    putStrLn ""
    let percBad = 100*(fromIntegral . length $ results)/(fromIntegral . length $ xs)
    putStrLn $ printf "%d bad values out of %d tested (%.1f%% good, %.1f%% bad)"
        ((length results)::Int) ((length xs)::Int) ((100-percBad)::Double) ((percBad)::Double)
    where
        showRow (v,ytest,ytrue,ae,re) = printf "%9.3e | %9.3e | %9.3e | %9.3e | %9.3e | %016X | %016X | %016X"
            (ae::Double) (re::Double) (v::Double) (ytest::Double) (ytrue::Double)
            ((fromIntegral $ dbl2Word64 v)::Integer) ((fromIntegral $ dbl2Word64 ytest)::Integer) ((fromIntegral $ dbl2Word64 ytrue)::Integer)
        isBad (_,_,_,ae,re) = ae >= aemin || (abs re) >= remin

fillRight :: Int -> [Char] -> [Char]
fillRight n = padLeftTo n ' '

dabserr :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double
dabserr f g x = abs (dtest f x - g x)
dabsrelerr :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double
dabsrelerr f g x = abs (dabserr f g x / g x)
drelerr :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double
drelerr f g x = (dtest f x - g x) / abs (g x)
dabs :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> [Double] -> [(Double, Double)]
dabs f g e x = filter ((> e) . snd) (zip x (map (dabserr f g) x))
derr :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> [Double] -> [(Double, Double)]
derr f g e x = filter ((> e) . snd) (zip x (map (dabsrelerr f g) x))

dtest :: (Interp VR -> Interp VR) -> Double -> Double
dtest f x = case doubles (f xs) of
              [a, b] -> if undoubles [b, a] == f xs
                        then a
                        else error $ "input " ++ show x ++ " gave output " ++ show fxs
                                     ++ " (" ++ showHex (integer @Interp xs) ")"
              _      -> error "Unexpected number of elements in the list."
  where
    fxs = doubles $ f xs
    xs = undoubles2 x



-- Randomly test a function and output a report every N tests.

data FPFormat = FP { sigBits :: Integer, expBits :: Integer }

fp32 :: FPFormat
fp32 = FP {sigBits = 23, expBits = 8}
fp64 :: FPFormat
fp64 = FP {sigBits = 52, expBits = 11}

randTest64 :: (Interp VR -> Interp VR) -> (Double -> Double) -> Int -> (Double, Double) -> IO ()
randTest64 f g n (low,high) = randTest f g n fp64 (low,high)
randTest32 :: (Interp VR -> Interp VR) -> (Double -> Double) -> Int -> (Double, Double) -> IO ()
randTest32 f g n (low,high) = randTest f g n fp32 (low,high)

singleTest :: (Interp VR -> Interp VR) -> (Double -> Double) -> FPFormat -> Double -> (Double, Double, Double, Double, Double)
singleTest f g fp v = (v, dtest f v, g v, (dabserr f g v) / (ulp fp $ g v), (drelerr f g v) / (eps fp))

randTest :: (Interp VR -> Interp VR) -> (Double -> Double) -> Int -> FPFormat -> (Double, Double) -> IO ()
randTest f g n fp (low,high) =
    (putStrLn $ printf "%10s | %14s | %9s | %9s | %9s | %16s | %16s | %16s"
        "#" "error" "xtest" "ytest" "ytrue" "xtest" "ytest" "ytrue") >>
    runTests 1 (0,0) (0,0)
    where
        runTests i (gmav,gmae) (gmrv,gmre)  = do
            results <- replicateM n doTest
            let (mav,mae,_) = maximumBy (compare `on` getAbs) results
            let (mrv,_,mre) = maximumBy (compare `on` getRel) results
            let ytest = dtest f gmav
            let ytrue = g gmav
            putStrLn $ printf "%10d | %9.3e ulps | %9.3e | %9.3e | %9.3e | %016X | %016X | %016X"
                ((i*n)::Int) (gmae::Double) (gmav::Double) (ytest::Double) (ytrue::Double)
                ((fromIntegral $ dbl2Word64 gmav)::Integer) ((fromIntegral $ dbl2Word64 ytest)::Integer) ((fromIntegral $ dbl2Word64 ytrue)::Integer)
            runTests (i+1) (maximumBy (compare `on` snd) [(mav,mae),(gmav,gmae)]) (maximumBy (compare `on` snd) [(mrv,mre),(gmrv,gmre)])
        doTest = do
            v <- randomRIO (low,high)
            let (_,_,_,ae,re) = singleTest f g fp v
            return (v,ae,re)
        getAbs (_,x,_) = x
        getRel (_,_,x) = x

ulp64 :: Double -> Double
ulp64 = ulp fp32
ulp32 :: Double -> Double
ulp32 = ulp fp64

eps32 :: Double
eps32 = eps fp32
eps64 :: Double
eps64 = eps fp64

eps :: FPFormat -> Double
eps (FP sigb _) = 2**(fromIntegral $ -sigb-1)

ulp :: FPFormat -> Double -> Double
ulp (FP sigb expb) x =
    if abs x < minNormal then ulp (FP sigb expb) minNormal else 2 ** (exponentVal - fromIntegral sigb)
    where
        bias = 2 ** (fromIntegral $ expb - 1) - 1
        minNormal = 2 ** (1 - bias)
        exponentVal = fromIntegral (floor (Prelude.log (abs x) / Prelude.log 2) :: Integer)


lst2Q :: Num a => [a] -> ([a], [a], [a], [a])
lst2Q xs = case xs of
  [x0, x1, x2, x3, x4, x5, x6, x7] -> ([x0,x1],[x2,x3],[x4,x5],[x6,x7])
  [x0, x1, x2, x3, x4, x5, x6]     -> ([x0,x1],[x2,x3],[x4,x5],[x6,0])
  [x0, x1, x2, x3, x4, x5]        -> ([x0,x1],[x2,x3],[x4,x5],[0,0])
  [x0, x1, x2, x3, x4]           -> ([x0,x1],[x2,x3],[x4,0],[0,0])
  [x0, x1, x2, x3]              -> ([x0,x1],[x2,x3],[0,0],[0,0])
  [x0, x1, x2]                 -> ([x0,x1],[x2,0],[0,0],[0,0])
  [x0, x1]                    -> ([x0,x1],[0,0],[0,0],[0,0])
  [x0]                       -> ([x0,0],[0,0],[0,0],[0,0])
  []                         -> ([0,0],[0,0],[0,0],[0,0])
  _                          -> error "lst2Q: List must have at most 8 elements."


worstError :: (Floating b, Ord b) => [[b]] -> (b, b)
worstError = (\ x->((/(Prelude.log 2)) $ Prelude.log x,x)) . maximum . map maximum . map (map abs)

timeStamp :: a -> IO (a,ClockTime)
timeStamp a = liftM (\ x -> (a,x)) getClockTime

testf2 :: (Interp VR -> Interp VR -> Interp VR) -> Double -> Double -> [String]
testf2 fun words1 words2 = take 1 $ zipWith4 (\w z x y->"  in1 "++(show w)++" in2 "++(show z)++" res"++(show x)++" "++(showHex y "")) 
                            (floats input1) (floats input2) (floats result) (wrds result)
  where
    result = fun input1 input2
    input1 = unfloats [words1]    
    input2 = unfloats [words2]  

cmpFltToHaskellFun :: (Interp VR -> Interp VR) -> (Double->Double) -> [Double] -> [[Double]]
cmpFltToHaskellFun f1 f2 lst 
  = map (\xs -> (zipWith (\x y -> (x-y)/(y+2**(-126))) 
                        (floats $ f1 (unfloats xs)) 
                        (map f2 xs) 
                )
        ) 
        $ permuteByFour lst
    

cmpFltToHaskellFun2 :: (Interp VR -> Interp VR -> Interp VR) -> (Double->Double->Double) -> [(Double,Double)] -> [[Double]]
cmpFltToHaskellFun2 f1 f2 lst 
  = map (\x -> (zipWith (\xs y -> (xs-y)/(y+2**(-126))) 
                        ((\ (x',y') -> floats $ f1 (unfloats x') (unfloats y')) $ unzip x) 
                        (map (uncurry f2) x) 
                )
        ) 
        $ permuteByFour lst
        
        
permuteByFour :: [a] -> [[a]]
permuteByFour (a1:a2:a3:a4:as) = [a1,a2,a3,a4]
                                 : permuteByFour (a2:a3:a4:as)
permuteByFour _ = []
