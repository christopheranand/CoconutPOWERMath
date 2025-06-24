module Sinc(sincSP,sincDev) where

import ISA.PowerISA
import Trig
import Sqrt
import Coconut.BaseTypes

sincSP :: PowerISA repr => repr Coconut.BaseTypes.VR -> repr Coconut.BaseTypes.VR
sincSP v = fst $ sincDev v
sincDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
sincDev v = 
  let
    sinvOverv = xvmulsp (sinFamily (Flags {accurateReduction = True, finalNudge = False, isCos = False, outputNaN = True, subnormalOutput = True}) v) (recipSP v)
    poly = xvmaddmsp  (xvmulsp v (unfloats4 (-0.16663411761070659801))) v (unfloats4 1)
    inside = xxland (xvcmpgtsp (unfloats4 (1/16)) v) (xvcmpgtsp v (unfloats4 (-1/16)))
    result = xxsel sinvOverv poly inside 

    isInfinity = vor (xvcmpgtsp v (unfloats4 (0x7f7fffff))) (vcmpgefp (xxland v (unfloats4 (0x7fffffff))) (unfloats4 (0x7fffffff))) -- 0x7f7fffff = biggest 32-bit floating point number

    isANumber = vcmpeqfp v v

    isZero = vcmpeqfp v (unfloats4 0)

    resultNaN = xxsel (unfloats4 $ 0/0) result isANumber

    resultInfinity =  xxsel resultNaN (unfloats4 $ 0/0) isInfinity

    resultZero =  xxsel resultInfinity (unfloats4 $ 0/0) isZero

  in
    (resultZero
    , [ ("v",v)
      , ("inside", inside)
      , ("isInfinity", isInfinity)
      , ("sinvOverv", sinvOverv)
      , ("result", result)
      , ("resultZero",resultZero)
      ]
    ) 
