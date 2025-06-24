module Sqrt(recipSP,recipDev, Sqrt.div, signMask, refineRecipSqrt, rsqrtSP, rsqrtDev, recipSqrtAbs, recipSqrtAbsDev, sqrt14bit, sqrt14bitDev, sqrtAbs14bit, sqrtSP, sqrtDev, sqrtAddSP, sqrtSubSP, sqrtNegAddSP, sqrtAbs, sqrtAbsDev, div2, hypotSP) where

import ISA.PowerISA
------------------
import Coconut.BaseTypes
--import Coconut.Utils.ArbFloat (saturate)
-------------
--import Data.Bits as Bits (xor, rotateL, complement, shift, shiftR, shiftL, (.&.), rotate, (.|.))
--import Data.Int (Int64, Int8, Int16, Int32)
--import Data.Word (Word64, Word32, Word16, Word8)
-- import SPUType as SPUType

-- We want our polynomials to $2^{ -23} = 0.119209 10^{ -6}$


-- **************** the current implementation will and with mask twice !!!!

-- | Better than 13-bit accurate reciprocal estimate, according to ISA:
-- frecip14bit ::SPUType a => VR a -> VR a
-- frecip14bit v = fi v (frest v)

-- div :: SPUType a => VR a -> VR a -> VR a
--unwrds' -> unwrds
----------------------------------------------V
-- TODO:  doesn't work for subnormals, 0 or infinity
recipSP :: PowerISA repr => repr VR -> repr VR
recipSP v = fst $ recipDev v 
recipDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
recipDev v = 
  let
    absv = xxland v signMask
    maybeSubnormal = (xvcmpgtsp absv (unfloats4 $ 1))
    adjustBefore = xxsel (unfloats4 $ 2**32)  (unfloats4 $ 2**(-32)) maybeSubnormal
    isZero = (xvcmpgtsp absv (unfloats4 $ 2**128-2**104))
    isInfinity = (vcmpgefp (unfloats4 $ 2**(-128)) absv)
    isANumber = vcmpeqfp v v  
    vAftermul = xvmulsp adjustBefore absv
    est = vrefp vAftermul                      -- hw estimate of 1/x
    one = unfloats [1,1,1,1]      
    vEst1 = xvnmsubmsp vAftermul est one       -- NR-iteration
    normalResult = xvmaddmsp vEst1 est est
    resultSubnormal = xvmulsp adjustBefore normalResult
    resultInfinity = xxsel resultSubnormal (unfloats4 $ 1/0) isInfinity
    resultZero = xxsel resultInfinity (unfloats4 $ 0) isZero
    resultPos     = xxsel (unfloats4 $ 0/0) resultZero isANumber
    result = xxsel v resultPos signMask

  in
    ( result
    , [ ("v",v)
      , ("vAftermul", vAftermul) 
      , ("isANumber", isANumber)
      , ("normalResult",normalResult)
      , ("resultSubnormal", resultSubnormal)
      , ("resultInfinity", resultInfinity)
      , ("resultZero", resultZero)
      , ("resultPos", resultPos)
      , ("result",result)
      ]
    )
{-recipDev v = 
  let
    zeroIfSmall = vcmpbfp v (unfloats4 1) 
    fffIfSmall = vcmpnezw zeroIfSmall zeroIfSmall
    maybeTooBig = xvmulfp v (unfloats4 $ 2^64) -- multiply subnormals by big number
    notSmall = xxsel v maybeTooBig fffIfSmall   -- pick either the input, v or 2^64 * v
    est = vrefp notSmall                      -- hw estimate of 1/x
    one = unfloats [1,1,1,1]           
    vEst1 = xvnmsubmsp v est one       -- NR-iteration
    result = xvmaddmsp vEst1 est est
  in
    ( result
    , [ ("v",v)
      , ("est",est)
      , ("vEst1",vEst1)
      , ("result",result)
      ]
    )-}

-- exponent bytes 
-- case   |   input   |   estimate
-- 0      |    00     |     ff  (infinity)
-- inf    |    ff     |     00  (0)
-- NaN    |    ff     |     ff
-- normal |  0<?<ff   |     <ff
-- VVVV not possible VVVV
-- subn   |    00     |     >f0 (normal or infinite)

-- 
 
--------------------------------------------------------------------------------V
div :: PowerISA repr => repr VR -> repr VR -> repr VR
div x y = xvmulsp ( x ) (recipSP ( y ))

-- vrsqrtefp replace recipSqrt14bit and fi 

signMask :: PowerISA repr => repr VR
signMask = unwrds4 0x7fffffff

--refinement code
refineRecipSqrt :: PowerISA repr => repr VR -> repr VR -> repr VR
refineRecipSqrt v est = xvmaddmsp (xvnmsubmsp (xvmulsp v est) est one) (xvmulsp est half) est
  where
    one = unfloats [1,1,1,1]
    half = unfloats [0.5,0.5,0.5,0.5]

--rsqrtSP v = refineRecipSqrt v (vrsqrtefp v)
--TODO:  doesn't work for 0 or infinity, get the same as est when input is 0.5^130
rsqrtSP :: PowerISA repr => repr VR -> repr VR
rsqrtSP v = fst $ rsqrtDev v
rsqrtDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
rsqrtDev v = 
  let
    absv = xxland v signMask
    maybeSubnormal = (xvcmpgtsp absv (unfloats4 $ 1))
    adjustBefore = xxsel (unfloats4 $ 2**32)  (unfloats4 $ 2**(-32)) maybeSubnormal
    adjustAfter = xxsel (unfloats4 $ 2**16)  (unfloats4 $ 2**(-16)) maybeSubnormal
    isZero = (xvcmpgtsp absv (unfloats4 $ 2**128-2**104))
    isInfinity = (vcmpgefp (unfloats4 $ 2**(-160)) absv)
    isANumber = xxland (vcmpeqfp v v) (vcmpgefp absv (unfloats4 $ 0))
    vAftermul = xvmulsp adjustBefore absv
    est = vrsqrtefp vAftermul
    normalResult = refineRecipSqrt vAftermul est
    resultSubnormal = xvmulsp adjustAfter normalResult
    resultInfinity = xxsel resultSubnormal (unfloats4 $ 1/0) isInfinity
    resultZero = xxsel resultInfinity (unfloats4 $ 0) isZero
    resultPos     = xxsel (unfloats4 $ 0/0) resultZero isANumber
    result = xxsel v resultPos signMask
  in
    ( result
    , [ ("v",v)
      , ("vAftermul", vAftermul) 
      , ("est",est)
      , ("normalResult",normalResult)
      , ("resultSubnormal", resultSubnormal)
      , ("resultInfinity", resultInfinity)
      , ("resultZero", resultZero)
      , ("resultPos", resultPos)
      , ("result",result)
      ]
    )
--Version which takes negative arguments:
--recipSqrtAbs v = refineRecipSqrt (xxland v signMask) (vrsqrtefp (xxland signMask v))
-- TODO:  doesn't work for  +-0 or +-infinity, get the same as est when input is 0.5^130
recipSqrtAbs :: PowerISA repr => repr VR -> repr VR
recipSqrtAbs v = fst $ recipSqrtAbsDev v
recipSqrtAbsDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
recipSqrtAbsDev v = 
  let
    est = vrsqrtefp (xxland signMask v)
    vSM = (xxland v signMask)
    result = refineRecipSqrt vSM est
  in
    ( result
    , [ ("v",v)
      , ("vSM",vSM)
      , ("est",est)
      , ("result",result)
      ]
    )

sqrt14bit :: PowerISA repr => repr VR -> repr VR
sqrt14bit v = fst $ sqrt14bitDev v
sqrt14bitDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
sqrt14bitDev v =
  let
    est = vrsqrtefp v
    result = xvmulsp v est
  in
    ( result
    , [ ("v",v)
      , ("est",est)
      , ("result",result)
      ]
    )

--Version which takes negative arguments:

--sqrtAbs14bit :: PowerISA repr => repr VR -> repr VR
sqrtAbs14bit :: PowerISA repr => repr VR -> repr VR
sqrtAbs14bit v = fst $ sqrtAbs14bitDev v
sqrtAbs14bitDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
sqrtAbs14bitDev v =
  let
    est = vrsqrtefp (xxland signMask v)
    result = xvmulsp (xxland signMask v) est
  in
    ( result
    , [ ("v",v)
      , ("est",est)
      , ("result",result)
      ]
    )


sqrtSP :: PowerISA repr => repr VR -> repr VR
sqrtSP v = fst $ sqrtDev v
sqrtDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
sqrtDev v = 
  let
    -- absv = xxland v signMask
    maybeSubnormal = (xvcmpgtsp v (unfloats4 $ 1))
    adjustBefore = xxsel (unfloats4 $ 2**(32))  (unfloats4 $ 2**(-32)) maybeSubnormal
    adjustAfter = xxsel (unfloats4 $ 2**(-16))  (unfloats4 $ 2**(16)) maybeSubnormal
    isZero = (xvcmpgtsp (unfloats4 $ 2**(-160)) v)
    isInfinity = (xvcmpgtsp v (unfloats4 $ 2**128-2**104) )
    isANumber = xxland (vcmpeqfp v v) (vcmpgefp v (unfloats4 $ 0))  
    vAftermul = xvmulsp adjustBefore v
    est = vrsqrtefp vAftermul
    refined = refineRecipSqrt vAftermul est
    normalresult = xvmulsp vAftermul refined
    resultSubnormal = xvmulsp adjustAfter normalresult
    resultInfinity = xxsel resultSubnormal (unfloats4 $ 1/0) isInfinity
    resultZero = xxsel resultInfinity (unfloats4 $ 0) isZero
    result     = xxsel (unfloats4 $ 0/0) resultZero isANumber    
  in
    ( result
    , [ ("v",v)
      , ("isANumber",isANumber)
      , ("normalResult",normalresult)
      , ("resultSubnormal", resultSubnormal)
      , ("resultInfinity", resultInfinity)
      , ("resultZero", resultZero)
      , ("result",result)
      ]
    )



sqrtAddSP :: PowerISA repr => repr VR -> repr VR -> repr VR
sqrtAddSP v v1 = xvmaddmsp v (refineRecipSqrt v (vrsqrtefp v)) v1



-- sqrtSPUsub :: PowerISA repr => repr VR -> repr VR -> repr VR
sqrtSubSP :: PowerISA repr => repr VR -> repr VR -> repr VR
sqrtSubSP v v1 = xvmsubmsp v (refineRecipSqrt v (vrsqrtefp v)) v1



-- sqrtSPUnegAdd :: PowerISA repr => repr VR -> repr VR -> repr VR
sqrtNegAddSP :: PowerISA repr => repr VR -> repr VR -> repr VR
sqrtNegAddSP v v1 = xvnmsubmsp v (refineRecipSqrt v (vrsqrtefp v)) v1



--Version which takes negative arguments:

sqrtAbs :: PowerISA repr => repr VR -> repr VR
--xvmulsp (xxland signMask v) $ refineRecipSqrt (xxland signMask v) (vrsqrtefp v)
sqrtAbs v = xvmulsp (xxland signMask v) 
               $ refineRecipSqrt (xxland signMask v) (vrsqrtefp (xxland signMask v))
sqrtAbsDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
sqrtAbsDev v = 
  let
    absolute = (xxland signMask v)
    result = xvmulsp absolute $ refineRecipSqrt absolute (vrsqrtefp absolute)
  in
    (result,
    [("v",v)
    ,("abs",absolute)
    , ("result",result)])

div2 :: PowerISA repr => repr VR -> repr VR -> repr VR
div2 x y = result
   where
      result = xvmulsp x yInv
      yInv = recipSP y


--hypotSPU :: PowerISA repr => repr VR -> repr VR -> repr VR
--floats @Interp $ Sqrt.div (unfloats4 $ 1/0) (unfloats [1/0,0,0/0,-1/0])
hypotSP :: PowerISA repr => repr VR -> repr VR -> repr VR
hypotSP x y = sqrtSP $ xvmaddmsp x x $ xvmulsp y y
