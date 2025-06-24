{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Pow(powSP,powDev,powFun,expCoeffs24bits) where

import Coconut.BaseTypes

import Log
import MathUtils
import ISA.PowerISA
-- import Data.Bits as Bits (xor, rotateL, complement, shift, shiftR, shiftL, (.&.), rotate, (.|.), countLeadingZeros)

data PowFlags = PowFlags {subnormalOutputPow :: Bool, outputNaNPow :: Bool}


-- | Works for y>0 and sometimes for other values, saturation is within a power of 10
powSP :: forall repr. (PowerISA repr) => repr VR -> repr VR -> repr VR
powSP y x = fst $ powDev y x
powDev :: forall repr. (PowerISA repr) => repr VR -> repr VR -> (repr VR, [(String, repr VR)])
powDev y x = powFun (PowFlags { subnormalOutputPow=True, outputNaNPow=True }) y x
powFun :: forall repr. (PowerISA repr) => PowFlags -> repr VR -> repr VR -> (repr VR, [(String, repr VR)])
powFun flags y x =
  let

    isBaseInfinity = xvcmpgtsp y (unfloats4 (0x7f7fffff)) -- 0x7f7fffff = biggest 32-bit floating point number
    isBaseNegativeInfinity = vor (xvcmpgtsp y (unfloats4 (0x7f7fffff))) (vcmpgefp (xxland y $ unfloats4 (0x7fffffff)) (unfloats4 (0x7fffffff)))

    isExpInfinity = xvcmpgtsp x (unfloats4 (0x7f7fffff)) -- 0x7f7fffff = biggest 32-bit floating point number
    isExpNegativeInfinity = vor (xvcmpgtsp x (unfloats4 (0x7f7fffff))) (vcmpgefp (xxland x $ unfloats4 (0x7fffffff)) (unfloats4 (0x7fffffff)))

    isBaseZero = vcmpeqfp y (unfloats4 0)
    isExpZero = vcmpeqfp x (unfloats4 0)

    isExpNeg = vcmpgefp (unfloats4 0) x

    isBaseSubnormal = xvcmpgtsp (unfloats4 $ 2**(-125)) y
    isExpNegOne = vcmpeqfp x (unfloats4 (-1))

    isSpecial1 = xxland (isBaseZero) (isExpNegativeInfinity)
    isSpecial2 = xxland (isBaseOne) (isExpANumber)
    -- isSpecial3 = xxland (isBaseNegativeInfinity) (isEvenExponent)
    isSpecial4 = xxland (isBaseInfinity) (isExpNeg)
    isSpecial5 = xxland (isExpANumber) (isBaseOne)
    isSpecial6 = xxland (isExpNegOne) (isBaseSubnormal)

    isBaseOne = vor (vcmpeqfp y (unfloats4 1)) (vcmpeqfp y (unfloats4 (-1)))
    -- isExpOne = vor (vcmpeqfp x (unfloats4 1)) (vcmpeqfp x (unfloats4 (-1)))
 
    isBaseANumber = vcmpeqfp y y
    isExpANumber = vcmpeqfp x x

    result = xvmaddmsp exp2 evalPoly fudge -- combine the exponential with the mantissa
    fudge = xvmulsp exp2 (unfloats4 $ 1*2**(-24))
    nonPositive = xvcmpgtsp (unwrds4 0x00800000) y -- is y denormal, zero or negative
    vlog2 = xvmaddmsp x (log2SP y) fudge2

    fudge2 = xvmulsp (xvmulsp x (log2SP y)) (unfloats4 $ 1*2**(-24))
    vBylog2AsInt = vctsxs domainmin 23
    
    restrictdomainmin = xvcmpgtsp (unfloats4 (-127)) vlog2 
    domainmin = xxsel vlog2 (unfloats4 (-127)) restrictdomainmin
    restrictDomainmax = xvcmpgtsp vlog2 (unfloats4 (129-128*2**(-23)))

    -- | Transform the integer exponent to a biased floating point exponent
    expnt = xxsel (unwrds4 0x00000000) vBylog2AsInt (unwrds4 0xff800000)
    exp2 = vadduwm expnt (unwrds4 0x03f800000)
    
    frac = xxsel (unfloats4 1) vBylog2AsInt (unwrds4 0x000fffff)
    
    coeffs = lookup8Word (22,20) expCoeffs24bits vBylog2AsInt
    
    evalPoly = hornerV coeffs frac

    intermediate = xxsel result (unwrds4 0x7fffffff) restrictDomainmax

    result1 = xxsel intermediate (unfloats4 0) nonPositive

    resultBaseZero =  xxsel result1 (unfloats4 $ 0) isBaseZero
    resultBaseNegativeInfinity =  xxsel resultBaseZero (unfloats4 $ -1/0) isBaseNegativeInfinity
    resultBaseInfinity =  xxsel resultBaseNegativeInfinity  (unfloats4 $ 1/0) isBaseInfinity
    -- resultEvenExponent =  xxsel resultBaseNegativeInfinity (unfloats4 $ 1/0) isSpecial3
    resultExpNegativeInfinity = xxsel resultBaseInfinity (unfloats4 $ 0) isExpNegativeInfinity
    resultExpInfinity =  xxsel resultExpNegativeInfinity (unfloats4 $ 1/0) isExpInfinity
    resultBaseOne = xxsel resultExpInfinity (unfloats4 $ 1) isBaseOne
    resultSpecial1 = xxsel resultBaseOne (unfloats4 $ 1/0) isSpecial1
    resultisBaseNumber = xxsel (unfloats4 $ 0/0) resultSpecial1 isBaseANumber
    resultisExpNumber = xxsel (unfloats4 $ 0/0) resultisBaseNumber isExpANumber
    resultExpNeg =  xxsel resultisExpNumber (unfloats4 $ 0) isSpecial4
    resultExpZero =  xxsel resultExpNeg (unfloats4 $ 1) isExpZero
    resultSpecial6 =  xxsel resultExpZero (unfloats4 $ 1/0) isSpecial6
    resultSpecial2 = xxsel resultSpecial6 (unfloats4 $ 1) isBaseOne

    result2     = if outputNaNPow flags then 
                        xxsel (resultSpecial2) (unfloats4 $ 1) (isSpecial5)
                     else 
                        resultBaseZero
  in
    ( result2
    , [ ("x",x)
    , ("y",y)
    , ("isBaseSubnormal",isBaseSubnormal)
    , ("isExpNegOne",isExpNegOne)
    , ("isExpZero",isExpZero)
    , ("isSpecial1",isSpecial1)
    , ("isSpecial2",isSpecial2)
    -- , ("isSpecial3",isSpecial3)
    , ("isSpecial4",isSpecial4)
    , ("isBaseInfinity",isBaseInfinity)
    , ("isBaseNegInfinity",isBaseNegativeInfinity)
    , ("isExpInfinity",isExpInfinity)
    , ("isExpNegInfinity",isExpNegativeInfinity)
    , ("isExpNeg",isExpNeg)
    , ("isBaseANumber",isBaseANumber)
    , ("isExpANumber",isExpANumber)
    , ("result",result)
    -- , ("fudge",fudge)
    , ("nonPositive",nonPositive)
    , ("vlog2",vlog2)
    -- , ("fudge2",fudge2)
    , ("vBylog2AsInt",vBylog2AsInt)
    , ("restrictdomainmin",restrictdomainmin)
    , ("domainmin",domainmin)
    , ("restrictDomainmax",restrictDomainmax)
    , ("exponent",expnt)
    , ("exp",exp2)
    , ("frac",frac)
    -- , ("coeffs",coeffs)
    , ("evalPoly",evalPoly)
    , ("intermediate",intermediate)
    , ("resultBaseZero",resultBaseZero)
    , ("resultExpZero",resultExpZero)
    , ("resultBaseInfinity",resultBaseInfinity)
    , ("resultBaseNegInfinity",resultBaseNegativeInfinity)
    , ("resultExpInfinity",resultExpInfinity)
    , ("resultExpNegInfinity",resultExpNegativeInfinity)
    , ("resultisBaseNumber",resultisBaseNumber)
    , ("result",result1)
    , ("result1",result1)
    , ("result2",result2)
    -- , ("resultSpecial2",resultSpecial2)
    ]
    )
  
expCoeffs24bits :: Floating b => [[b]]
expCoeffs24bits = map (map (*(1+1.75*2**(-24))))
  [[0.48891044350695485758, 0.53316061922513474670, 0.58141577801760612158, 0.63403840182178655684,

    0.69142377999338053530, 0.75400297863142253485, 0.82224607865020398485, 0.89666570692173328523],

    [0.38699665481517968710, 0.42202284459154137020, 0.46021917538846515929, 0.50187256948194926125,

    0.54729591783264606172, 0.59683043045263231840, 0.65084819949852790587, 0.70975499434440434852],

    [0.066123238148862889030, 0.072107902510200950180, 0.078634225273645770070,

    0.085751230713052959530, 0.093512380178148102350, 0.10197597368420169445, 0.11120558784869257362,

    0.12127055346458543413], [0.057969644369562528236, 0.063216345444863077065,

    0.068937913538461603074, 0.075177327787501201973, 0.081981457273380400124,

    0.089401413091788440470, 0.097492932287795729211, 0.10631679654005143476]
  ]
