{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Activation(geluOne, geluOneDev, geluTwo, geluTwoDev, geluThree, geluThreeDev, silu, siluDev, sigmoid, sigmoidDev, softplus, softplusDev, halfTanhC) where
import Sqrt
import Exp
import Coconut.BaseTypes
import Coconut.Graph.CodeGraph
import Coconut.Utils.CGWrappers
import Coconut.Graph.Dot

-- import TestUtils 
-- import ISA.PowerISA

import Hyperbolic
-- import Pow
import MathUtils
import Log

import ISA.PowerISA

-- import Coconut.BaseTypes


-- GELU (https://paperswithcode.com/method/gelu)

-- GELU 1 | 0.5 * x * (1+tanh(sqrt(2/π)∗(x+0.044715∗x^3))) 
-- Using 2*e^(2x) / (1 + e^(2x)) to calculate 1+tanh x, calculate e ignoring exceptions 
-- Note that the 0.5 will cancel with the 2 from 2*e^(2x)

geluOne :: PowerISA repr => repr VR -> repr VR
geluOne x = fst $ geluOneDev x
geluOneDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
geluOneDev x = 
    let
        simplify = div2 (xvmulsp (unfloats4 2) (expSP (xvmulsp (x) (unfloats4 2)))) (xvaddsp (unfloats4 1) (expSP (xvmulsp (x) (unfloats4 2))))
        result = xvmulsp (xvmulsp (unfloats4 0.5) (x)) (simplify)
    in
        ( result
        , [
         ("simplify",simplify)
        , ("result",result)
        ]
    )

-- GELU 2 | Use tanh function in PowerMath Hyperbolic.hs with exceptions turned off 
-- Can make a new table with values multiplied by 0.5 so that we don't need to multiply by 0.5 when evaluating 
-- Calculate 0.5 * x * (1+tanh(sqrt(2/π)*x*(1+0.044715∗x^2))) 


--   = (0.5 * x + x * halftanh(x*(sqrt(2/pi) + 0.044715 * (x^2)))

-- Maybe try x*(tanh(0.0356774 x^3 + 0.797885 x) + 1) too

-- Unroll twice because of the tanh evaluation which uses use16X2lookup which wants to be unrolled twice as it shares calculations 

geluTwo :: (PowerISA repr) => (repr VR,repr VR) -> (repr VR,repr VR)
geluTwo     x = fst $ geluTwoDev x

geluTwoDev :: (PowerISA repr) => (repr VR,repr VR) -> ((repr VR,repr VR), [(String, repr VR)])
geluTwoDev  (x0,x1) = 
    let 
        x2 = pMapPP xvmulsp (x0,x1) (x0,x1)
        halfX = pMapoP xvmulsp (unfloats4 0.5) (x0,x1)
        sqrt2oPi = unfloats4 (sqrt $ 2 / pi)
        oneP44x2 = pMapoPo xvmaddmsp (unfloats4 0.044715) x2 (unfloats4 1)
        sqrtx = pMapoP xvmulsp sqrt2oPi (x0,x1)
        sqrtxoneP44x2 = pMapPP xvmulsp oneP44x2 sqrtx 
        halfTanh = use16X2lookup tanhLookup halfTanhC tanhKeyResult sqrtxoneP44x2
        result = pMapPPP xvmaddmsp (x0,x1) halfTanh halfX  
    in
        ( result
        , [
          ("x", x0)
        , ("x2",fst x2)
        , ("halfX",fst halfX)
        , ("sqrt2oPi", sqrt2oPi)
        , ("oneP44x2",fst oneP44x2)
        , ("sqrtx",fst sqrtx)
        , ("sqrtxoneP44x2",fst sqrtxoneP44x2)
        , ("halfTanh", fst halfTanh)
        , ("result",fst result)
        ]
    )

-- GELU 3 | x*sigmoid(1.702*x)
-- Use the wikipedia definition of sigmoid (don't turn off exception for the exp in the sigmoid)
geluThree :: (PowerISA repr) => repr VR -> repr VR
geluThree x = fst $ geluThreeDev x

geluThreeDev :: (PowerISA repr) => repr VR -> (repr VR, [(String, repr VR)])
geluThreeDev x = 
    let 
        inside = xvmulsp x (unfloats4 1.702)
        result = xvmulsp x $ sigmoid inside
    in 
        ( result , 
            [ 
                  ("x", x)   
                , ("inside",inside)
                , ("result",result)
            ]
        )

-- SiLU (https://paperswithcode.com/method/silu)
-- x*sigmoid(x)
silu :: (PowerISA repr) => repr VR -> repr VR
silu x = fst $ siluDev x

siluDev :: (PowerISA repr) => repr VR -> (repr VR, [(String, repr VR)])
siluDev x = 
    let 
        result = xvmulsp x $ sigmoid x
    in 
        ( result , 
            [ 
                  ("x", x)   
                , ("result", result)
            ]
        )
-- ReLU (https://paperswithcode.com/method/relu)
-- max(0,x)
relu :: (Num a, Ord a) => a -> a
relu x = if x >= 0 then x else 0

-- Leaky ReLU (https://paperswithcode.com/method/leaky-relu)

-- Sigmoid (https://paperswithcode.com/method/sigmoid-activation)
-- 1/(1+e^-x) (don't turn off exception for the exp in the sigmoid)
sigmoid :: forall repr. (PowerISA repr) => repr VR -> repr VR
sigmoid x = fst $ sigmoidDev x
sigmoidDev :: forall repr. (PowerISA repr) => repr VR -> (repr VR, [(String, repr VR)])
sigmoidDev x = 
    let 
        negx = vxor x signBit
        expon = expSP $ negx
        denominator = xvaddsp (unfloats4 1) (expon)
        result = recipSP denominator
    in 
        ( result , 
            [ 
                  ("x", x)   
                , ("negx", negx)   
                , ("expon", expon)   
                , ("denominator", denominator)
                , ("result",result)
            ]
        )

-- Softplus (https://paperswithcode.com/method/softplus)
-- log(1 + e^x)
softplus :: PowerISA repr => repr VR -> repr VR
softplus x = fst $ softplusDev x
softplusDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
softplusDev x = 
    let 
        expon = expSP x
        poly = xvaddsp (unfloats4 1) (expon)
        result = logeSP poly
    in 
        ( result , 
            [ 
                  ("x", x)     
                , ("expon", expon)   
                , ("poly", poly)
                , ("result",result)
            ]
        )

-- Softmax? (https://en.wikipedia.org/wiki/Softmax_function) (https://paperswithcode.com/method/softmax)
-- Later


-- tanhC * 0.5
halfTanhC :: [[Double]]
halfTanhC = [[0.0,3.8332999387932815e-7,4.261223143837068e-5,4.380241930897725e-4,1.4413489213249595e-3,-1.4759094583171574e-5,-2.627423815674419e-2,-7.7613143158711e-2,-0.11119567709436391,-7.409425356855576e-2,8.481368471729828e-2,0.25030643557400056,0.3665786276573405,0.45299644283913787,0.49096881149934823,0.49852282282957]
  ,[0.5000000298023224,0.49998474590202624,0.4992134530474566,0.4945511553704766,0.485824908603588,0.49397442410462683,0.6138231982791704,0.7979697117958473,0.8972310314422391,0.8141801737482824,0.5220688903782856,0.2751003829492666,0.12978488727635415,3.878102506853301e-2,6.154405074841827e-3,8.55893480990419e-4]
  ,[1.9706747459164154e-8,2.4411368826042612e-4,5.963147571474684e-3,2.8134916509957336e-2,5.8632055313017344e-2,4.072851175579735e-2,-0.17909457069096762,-0.4441315824080821,-0.5616843508604672,-0.4873483988697854,-0.27184692972560304,-0.12410045001272009,-5.13447189829302e-2,-1.2927012804132568e-2,-1.6879573311745098e-3,-1.9918395265863996e-4]
  ,[-0.1666688189449533,-0.16863667232267207,-0.19022348189588706,-0.2434031382880719,-0.2969376467501206,-0.27778294372692464,-7.520544770051159e-2,0.11611973346871579,0.18583916503755943,0.15258481746279587,7.282956867005161e-2,2.8540124684414924e-2,1.0299491649680845e-2,2.1726651709045733e-3,2.3271104360391166e-4,2.3261988602147536e-5]
  ,[6.843722861985052e-5,8.266946428530679e-3,4.986147618856656e-2,0.11421177792565752,0.16140922784449813,0.15155480685877118,5.774640117770519e-2,-1.1522317928090912e-2,-3.222911901802742e-2,-2.479432769010583e-2,-9.987485013932644e-3,-3.335199371350177e-3,-1.045301399338238e-3,-1.8387801316569665e-4,-1.6115347163120812e-5,-1.3627266763781626e-6]
  ,[6.593336730032276e-2,5.137874366590532e-2,1.8579637808906816e-2,-1.2850205016596809e-2,-2.9566907587237148e-2,-2.76629207379376e-2,-1.0198844259190099e-2,-1.374223218859886e-4,2.326170283478894e-3,1.661636369599258e-3,5.585077204639796e-4,1.5801464804917335e-4,4.2863023134026825e-5,6.26208990874226e-6,4.481794030272343e-7,3.2023304353196056e-8]]
