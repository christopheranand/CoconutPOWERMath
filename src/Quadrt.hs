module Quadrt(quadrtSP,quadrtDev,qdrt1Release,qdrt1ReleaseDev,refineInvQdrtFudge,refineInvQdrt,Flags(..),recipQdrt,recipQdrtDev) where

import ISA.PowerISA
import Sqrt
import Coconut.BaseTypes

{- TODO
 -inf gives NaN, but expected is Infinity (which seems wrong)
 0 only gives NaN   
 inf only gives NaN
 in case of + subnormal recipQdrtDbg gives NaN

 -}
quadrtSP :: PowerISA repr => repr Coconut.BaseTypes.VR -> repr Coconut.BaseTypes.VR
quadrtSP v = fst $ quadrtDev v
quadrtDev :: PowerISA repr => repr Coconut.BaseTypes.VR -> (repr Coconut.BaseTypes.VR,[(String, repr Coconut.BaseTypes.VR)])
quadrtDev v = 
  let
    vsqrt = xvmulsp rsqrt v
    rsqrt = rsqrtSP v
    rqdrt = rsqrtSP vsqrt
    result = xvmulsp vsqrt rqdrt

    isInfinity = (xvcmpgtsp v (unfloats4 (0x7f7fffff)))  -- 0x7f7fffff = biggest 32-bit floating point number
    isANumber = vcmpeqfp v v
    isZero = vcmpeqfp v (unfloats4 0)
    isNegative = xvcmpgtsp (unfloats4 0) v
    isNegativeInfinity = (vcmpgefp (xxland v (unfloats4 (0xff800000))) (unfloats4 (0xff800000)))

    resultZero =  xxsel result (unfloats4 $ 0) isZero
    resultInfinity =  xxsel resultZero (unfloats4 $ 1/0) isInfinity
    resultNegative = xxsel resultInfinity (unfloats4 $ 0/0) isNegative
    resultNegInf = xxsel resultNegative (unfloats4 $ 0/0) isNegativeInfinity
    resultNaN = xxsel (unfloats4 $ 0/0) resultNegInf isANumber
    

  in 
    (resultNaN
    , [("v",v)
      ,("vsqrt",vsqrt)
      ,("rsqrt",rsqrt)
      ,("rqdrt",rqdrt)
      ,("isInfinity",isInfinity)
      ,("isNegativeInfinity",isNegativeInfinity)
      ,("isANumber", isANumber)
      ,("resultNaN",resultNaN)
    ]
    )

  
-- x^1/4 sqrt(sqrt(x))
qdrt1Release :: PowerISA repr => repr VR -> repr VR
qdrt1Release v = fst $ qdrt1ReleaseDev v
qdrt1ReleaseDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
qdrt1ReleaseDev v = 
  let 
    vrqdrt = refineInvQdrt v (refineInvQdrt v rsqrtesqrt)
    vrsqrt = rsqrtSP v
    vsqrt = xvmulsp v vrsqrt
    rsqrtesqrt = (vrsqrtefp  vsqrt)
    result = recipSP vrqdrt

    isInfinity = vor (xvcmpgtsp v (unfloats4 (0x7f7fffff))) (vcmpgefp (xxland v (unfloats4 (0x7fffffff))) (unfloats4 (0x7fffffff))) -- 0x7f7fffff = biggest 32-bit floating point number
    isANumber = vcmpeqfp v v
    isZero = vcmpeqfp v (unfloats4 0)

    resultZero =  xxsel result (unfloats4 $ 0) isZero
    resultInfinity =  xxsel resultZero (unfloats4 $ 1/0) isInfinity
    resultNaN = xxsel (unfloats4 $ 0/0) resultInfinity isANumber

    final = resultNaN

  in
    (final
    , [("v",v)
      ,("vrqdrt",vrqdrt)
      ,("vrsqrt",vrsqrt)
      ,("vsqrt",vsqrt)
      ,("rsqrtesqrt",rsqrtesqrt)
      ,("isInfinity",isInfinity)
      ,("isANumber", isANumber)
      ,("final",final)
    ]
    )

data Flags = Flags {subnormalOutput :: Bool, outputNaN :: Bool}

recipQdrt :: PowerISA repr => repr VR -> repr VR
recipQdrt v = fst $ recipQdrtDev (Flags {subnormalOutput=True, outputNaN=True}) v 
recipQdrtDev :: PowerISA repr => Flags -> repr VR -> (repr VR, [(String, repr VR)])
recipQdrtDev flags v = 
  let 
    maybeSubnormalInput = xvcmpgtsp (unfloats4 $ (2**(-125))) v

    adjustBefore = xxsel (unfloats4 1) (unfloats4 $ 2**32) maybeSubnormalInput
    adjustAfter  = xxsel (unfloats4 1) (unfloats4 $ 256) maybeSubnormalInput

    vForSubnormal = if subnormalOutput flags then 
                      xvmulsp adjustBefore v
                    else 
                      v

    vrsqrt = rsqrtSP vForSubnormal
    vsqrt = xvmulsp vForSubnormal vrsqrt
    rsqrtesqrt = (vrsqrtefp vsqrt)
    result = refineInvQdrt vForSubnormal (refineInvQdrt vForSubnormal rsqrtesqrt)

    resultSubnormal = if subnormalOutput flags then 
                        xvmulsp result adjustAfter
                      else 
                        result

    isPosInfinity = xvcmpgtsp v (unfloats4 (0x7f7fffff)) -- 0x7f7fffff = biggest 32-bit floating point number
    -- isANumber = vcmpeqfp v v
    isZero = vcmpeqfp v (unfloats4 0)
    isNegative = xvcmpgtsp (unfloats4 0) v
    -- isNegativeInfinity = xxland (xvcmpgtsp v (unfloats4 (0xff800000))) (vcmpgefp (xxland v $ unfloats4 (0xff800000)) (unfloats4 (0xff800000)))

    resultPosInfinity =  xxsel resultSubnormal (unfloats4 $ 0) isPosInfinity
    -- resultNegInfinity =  xxsel resultPosInfinity (unfloats4 $ 0/0) isNegativeInfinity
    resultNegative = xxsel resultSubnormal (unfloats4 $ 0/0) isNegative
    resultZero = xxsel resultNegative (unfloats4 $ 1/0) isZero

    final = resultZero

  in
    (final
    , [("v",v)
      ,("vForSubnormal", vForSubnormal)
      ,("vrsqrt",vrsqrt)
      ,("vsqrt",vsqrt)
      ,("rsqrtesqrt",rsqrtesqrt)
      ,("rsqrtesqrt",rsqrtesqrt)
      ,("resultSubnormal", resultSubnormal)
      ,("resultPosInfinity", resultPosInfinity)
      ,("result",result)
      ,("final",final)
    ]
    )

refineInvQdrtFudge :: PowerISA repr => repr VR -> repr VR -> repr VR
refineInvQdrtFudge a x = xvmaddmsp x (xvmaddmsp (xvmulsp (unfloats4 (-1/4)) a) 
                                    (xvmulsp (xvmulsp x x) (xvmulsp x x))
                                    (unfloats4 (5/4)) 
                               )
                             (xvmulsp x (unfloats4 $ 2**(-24)))

refineInvQdrt :: PowerISA repr => repr VR -> repr VR -> repr VR
refineInvQdrt a x = xvmulsp x (xvmaddmsp (xvmulsp (unfloats4 (-1/4)) a) 
                              (xvmulsp (xvmulsp x x) (xvmulsp x x))
                              (unfloats4 (5/4)) 
                         )
-- 5/4*x - 1/4*a*x*x*x*x*x
-- 5/4*x - 1/4*a*x*x*x*x*x

{-\textit{
Note that exceptional cases for arguments less than 
$1.4655059599601802e-39$ which should evaluate to 
maxFloat for reciprocals, but don't can be treated 
with 3 additional instructions, given in ISA.
}
-}
