{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Log(logeSP,logeDev,log10SP,log10Dev,log1pSP,log1pDev,log1pFun,log2SP,log2Dev,log2Fun) where

import ISA.PowerISA
import MathUtils
import Coconut.BaseTypes

logeSP :: PowerISA repr => repr VR -> repr VR
logeSP v = fst $ logeDev v
logeDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
logeDev v = 
        let
                -- result0 = xvmulsp (unfloats4 (log 2)) log2v
                
                -- result1 = xvmulsp (unfloats4 (log 2 * (1+2**(-24)))) log2v  -- best
                
                -- result2 = xvmaddmsp (unfloats4 (log 2)) log2v small
                -- small = xvmulsp (unfloats4 (log 2 * (2**(-24)))) log2v
                
                -- result3 = xvmaddmsp (unfloats4 (log 2)) log2v small3
                -- small3 = xvmulsp (unfloats4 (log 2 * (1.5*2**(-24)))) log2v
                
                -- result4 = xvmaddmsp (unfloats4 (log 2 * (1+2**(-24)))) log2v small4
                -- small4 = xvmulsp (unfloats4 (log 2 * (2**(-24)))) log2v
                
                -- result5 = xvmaddmsp (unfloats4 (log 2 * (1+2**(-24)))) log2v small5
                -- small5 = xvmulsp (unfloats4 (log 2 * (1.5*2**(-24)))) log2v
                
                result6 = xvmaddmsp (unfloats4 (log 2 * (1+2**(-24)))) log2v small6
                small6 = xvmulsp (unfloats4 (log 2 * (1.5*2**(-24)))) log2v
                
                result7 = xvmaddmsp (unfloats4 (log 2 * (1+1.5*2**(-24)))) log2v small7
                small7 = xvmulsp (unfloats4 (log 2 * (1.5*2**(-24)))) log2v
                
                log2v = (log2SP v)
        in
          ( result6
          , [ ("v",v)
            , ("result1",result6)
            , ("result2",result6)
            , ("result3",result6)
            , ("result4",result6)
            , ("result5",result6)
            , ("result6",result6)
            , ("result7", result7)
            , ("small7", small7)
            , ("log2v", log2v)
            ]
          )

log10SP :: PowerISA repr => repr VR -> repr VR
log10SP v = fst $ log10Dev v
log10Dev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
log10Dev v =
        let
                result = xvmulsp (unfloats4 (log 2/log 10)) (log2SP v)

                result6 = xvmaddmsp (unfloats4 ((log 2/log 10) * (1+2**(-24)))) log2v small6
                small6 = xvmulsp (unfloats4 ((log 2/log 10) * (1.5*2**(-24)))) log2v

                result7 = xvmaddmsp (unfloats4 ((log 2/log 10) * (1+1.5*2**(-24)))) log2v small7
                small7 = xvmulsp (unfloats4 ((log 2/log 10) * (1.5*2**(-24)))) log2v
                log2v = (log2SP v)
        in
          ( result6
          , [ ("v",v)
            , ("log2v" , log2v)
            , ("result",result)
            , ("result6",result6)
            , ("result7",result7)
            ]
          )

log1pSP :: PowerISA repr => repr VR -> repr VR
log1pSP v = fst $ log1pDev v
log1pDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
log1pDev v = log1pFun (Flags { subnormalOutput=True, outputNaN=True }) v
log1pFun :: PowerISA repr => Flags -> repr VR -> (repr VR, [(String, repr VR)])
log1pFun flags v =
      let
       
        vP1 = xvaddsp v $ unfloats4 1

        isZero = vcmpeqfp vP1 (unfloats4 0)

        isNegative = xvcmpgtsp (unfloats4 (0)) vP1

        isInfinity = xvcmpgtsp vP1 (unfloats4 (0x7f7fffff)) -- 0x7f7fffff = biggest 32-bit floating point number

        isANumber = vcmpeqfp vP1 vP1

        
        coeffs = lookup8Word (23,21) log1pCoeffs vP1
        log2Narrow = hornerV coeffs v
        log2v = xxsel log2Narrow log2Wide (xvcmpgtsp log2Wide $ unfloats4 (1 - 2**(-24)))
        log2Wide = log2SP vP1
        result6 = xvmaddmsp (unfloats4 (log 2 * (1+2**(-24)))) log2v small6
        small6 = xvmulsp (unfloats4 (log 2 * (1.5*2**(-24)))) log2v


        resultNegative = xxsel result6 (unfloats4 $ 0/0) isNegative
        resultInfinity =  xxsel resultNegative (unfloats4 $ 1/0) isInfinity

        resultZero =  xxsel resultInfinity (unfloats4 $ -1/0) isZero
        
        result     = if outputNaN flags then 
                            xxsel (unfloats4 $ 0/0) resultZero isANumber
                        else 
                            resultZero

        log1pCoeffs = map (map (*(1+2**(-24)))) [[0.31870535789879253990e-1, 0.14676784368001922360e-2, 0.26006964014097749000e-4, 0, 0, 0.10859796908339808820e-3, 0.11001817924494229762e-2, 0.40580620501616133210e-2], [1.8549857756466974549, 1.4701127721588926483, 1.4435533166593329949, 1.4426950609917314553, 1.4426947860912820093, 1.4407581723602833246, 1.4310137041133613471, 1.4113430654697975358], [1.4446813326376093804, -0.51246653691056269425, -0.70981340474134368557, -0.72133965682610766953, -0.72129448630746105005, -0.70681255895824208456, -0.66795082444425199358, -0.61527925026841903971], [6.3010834157706755095, 1.3014596402773916300, 0.56107404937956421585, 0.48138918570762369093, 0.47912278549674993422, 0.42113726884477313831, 0.34246851586423321889, 0.27147962390805503962], [7.7611126619593721664, 1.3424500334680608069, -0.62083560398372868480e-1, -0.35005771131109573666, -0.33945747057999561420, -0.21313053399681970664, -0.13228690732099906373, -0.84131774148079925101e-1], [5.2154265948675673294, 1.9010154309667059317, 0.82180278269497669759, 0.37958798261483399704, 0.17928159154677841926, 0.59406732217723407886e-1, 0.25681336959221172217e-1, 0.12530273653116925362e-1]]
      in
          ( result
          , [ ("v",v)
            , ("vP1", vP1)
            , ("log2v", log2v)
            , ("cmp", xvcmpgtsp log2Wide $ unfloats4 (1 - 2**(-24)))
            , ("log2Narrow", log2Narrow)
            , ("log2Wide", log2Wide)
            , ("isZero", isZero)
            , ("isNegative", isNegative)
            , ("isInfinity", isInfinity)
            , ("isANumber", isANumber)
            , ("result6",result6)
            , ("result",result)
            ]
          )

data Flags = Flags {subnormalOutput :: Bool, outputNaN :: Bool}
log2SP :: forall repr. (PowerISA repr) => repr VR -> repr VR
log2SP v = fst $ log2Dev v 
log2Dev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
log2Dev v = log2Fun (Flags { subnormalOutput=True, outputNaN=True }) v
log2Fun :: PowerISA repr => Flags -> repr VR -> (repr VR, [(String, repr VR)])
log2Fun flags v = 
  let
    -- if we want subnormal outputs, we need to make them normal outputs
    -- and then divide by a power of two
    maybeSubnormalInput = xvcmpgtsp (unfloats4 $ (2**(-125))) v

    adjustBefore = xxsel (unfloats4 1) (unfloats4 $ 2**32) maybeSubnormalInput
    adjustAfter  = xxsel (unfloats4 0) (unfloats4 $ 32) maybeSubnormalInput
    
    vForSubnormal = if subnormalOutput flags then 
                      xvmulsp adjustBefore v
                    else 
                      v

    isZero = vcmpeqfp vForSubnormal (unfloats4 0)

    isNegative = xvcmpgtsp (unfloats4 0) vForSubnormal

    isInfinity = xvcmpgtsp vForSubnormal (unfloats4 (0x7f7fffff)) -- 0x7f7fffff = biggest 32-bit floating point number

    isANumber = vcmpeqfp vForSubnormal vForSubnormal
    
    c0PexpPart = xvaddsp expAsFloat c0
    frac = onePlusMant 23 vForSubnormal
--This version reduces the number of even instructions by extracting the exponent and mantissa
--with |shufb|, and using a |rotqbii| to align the fraction.
    wrd0x00000001 = unwrds4 1       
    killSign = vslw vForSubnormal wrd0x00000001 -- shift the sign bit out and shift in 0
    expByte = vperm killSign wrd0x00000001 --Note: vperm would overwrite wrd0x00000001
                    $ unbytes [16,16,16,0
                              ,16,16,16,4
                              ,16,16,16,8
                              ,16,16,16,12
                              ]

    -- killSignZeros = (vclzw killSign)
    -- adjustedZeros = vnegw $ vadduwm killSignZeros (unint32s (replicate 4 (-8)))
    -- expBytePlusMant = xxsel expByte adjustedZeros maybeSubnormalInput --FIX Comment --count leading zeros of killSign and then subtract the 8 bits from the exponent and then xxsel 0 and this value 
    expAsFloat = vcfsx (vadduwm expByte (unint32s (replicate 4 (-127)))) 0 

    (offset:c0:coeffs, _lookupDbg) = lookup8WordDev (22, 20) log2OffsetsCoeffs vForSubnormal
    normalResult = hornerV (c0PexpPart:coeffs) fracMoffset
    fracMoffset = xvsubsp frac offset

    resultSubnormal = if subnormalOutput flags then 
                        xvsubsp normalResult adjustAfter
                      else 
                        normalResult
                        
    resultNegative = xxsel resultSubnormal (unfloats4 $ 0/0) isNegative
    resultInfinity =  xxsel resultNegative (unfloats4 $ 1/0) isInfinity

    resultZero =  xxsel resultInfinity (unfloats4 $ -1/0) isZero
    
    result     = if outputNaN flags then 
                        xxsel (unfloats4 $ 0/0) resultZero isANumber
                     else 
                        resultZero
    

  in
    ( result
    , [ ("v",v)
      , ("c0PexpPart", c0PexpPart)
      , ("frac", frac)
      , ("maybeSubnormalInput", maybeSubnormalInput)
      , ("adjustBefore", adjustBefore)
      , ("adjustAfter", adjustAfter)
      , ("expByte", expByte)
      , ("expAsFloat", expAsFloat)
      , ("fracMoffset", fracMoffset)
      , ("offset", offset)
      , ("normalResult", normalResult)
      , ("vForSubnormal", vForSubnormal)
      , ("isNegative", isNegative)
      , ("isInfinity", isInfinity)
      , ("result", result)
      ] -- ++ lookupDbg
    )

log2Offsets :: [Double]
log2Offsets = [1, 9/8, 5/4, 11/8, 3/2, 13/8, 7/4, 2]
log2OffsetsCoeffs :: [[Double]]
log2OffsetsCoeffs = log2Offsets : ([[0, (*(1+2**(-24))) 0.169925001560126755637549044713, (*(1+2**(-24))) 0.321928094951885621631914275209, (*(1+2**(-23))) 0.459431618674633002912244090330, (*(1+2**(-23))) 0.584962500743772265693705188270, (*(1+2**(-23))) 0.700439718155332450794887353211, (*(1+2**(-24))) 0.807354922066872765274074142292, 1], 
    map (*(1+2**(-23))) [1.44269502147602116658678140704, 1.28239551662334631293088247299, 1.15415599154273288858625928737, 1.04923273321648457663023887080, 0.961796679527191251506073960200, 0.887812323796096559135658531592, 0.824397160330456447605836596769, 0.721347521882741179925213343680], 
    map (*(1+2**(-24))) [-0.721341413698032490273340141084, -0.569945953444946429722260444462, -0.461658239878271749691388518914, -0.381536775566296263244524169459, -0.320597441874075339777263270628, -0.273172110270866530562229514793, -0.235541452698839568140571571696, -0.180336633798012193564514029993], 
    map (*(1+2**(-23))) [0.480551655694583929559637385303, 0.337469319570134868354498343756, 0.246066857796544819079410197824, 0.184900476111986494361332917852, 0.142435146190452311054378211846, 0.112037553000108653841689591204, 0.897085991249400712490406793785e-1, 0.601231395981801591694267388538e-1], 
    map (*(1+2**(-24))) [-0.353368424221827024657407798643, -0.220496920958797930844471732049, -0.145195258776278556037648402308, -0.994449270868254471820925461937e-1, -0.703659765404922828186387487058e-1, -0.511747294412329532941874530003e-1, -0.380991088954045644125001470474e-1, -0.223507239522998925651325001824e-1],  
    map (*(1+2**(-23))) [0.222111043759828540436359375012, 0.123600857707754366333919563212, 0.748289820382949659329693467466e-1, 0.474281280573398651448304386269e-1, 0.312305098322045340019913506874e-1, 0.212390362843837484947997315627e-1, 0.148488155024775245324434190140e-1, 0.104698014942068364785415197949e-1]])
