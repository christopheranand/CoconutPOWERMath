{-# OPTIONS -fwarn-unused-binds -fwarn-unused-matches #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, AllowAmbiguousTypes #-}
module Cbrt(cbrtSP, cbrtDev, rcbrtSP, rcbrtDev) where

import Coconut.BaseTypes
import ISA.PowerISA
--import ISA.PowerInterp
import MathUtils
import Sqrt (recipSP)

--import PrelExts (padLeftTo)

-- import Data.List
-- import qualified Data.List as List
import Data.Word
-- import Data.Bits as Bits
import Numeric


-- Cube Root is defined to be the unique real cube root with the same sign as the input.
-- We calculate it using

-- This function uses |divShiftMA| for fixed-point division.
-- This computation is inexact,
-- but |cbrtAssert| tests all the values which can occur
-- as a result of extracting the exponent bits for the input float.
-- If you modify the code you must modify the assertion.


-- cbrtSP :: forall a . (SPUType a, HasJoin String (VR a)) => VR a -> VR a
cbrtSP :: forall repr. PowerISA repr => repr VR -> repr VR
cbrtSP v = fst $ cbrtDev v 
{-}
assert cbrtAssert "cbrtSP" result
    where
-- Since we process the input in components,
-- we cannot rely on hardware to round denormals to zero,
-- and must detect it ourselves by comparing the biased exponent with zero

        denormal = vcmpequw exponent (unwrds4 0)

        result = xxsel unsigned (unwrds4 0) denormal

        -- We calculate the results of the exponent and mantissa 
        -- parts (based on polynomial evaluation)
        -- separately,
        -- and combine them using floating-point multiplication,

        unsigned = xvmulsp signCbrtExp evalPoly

        -- Put exponent bits into the last (low order) byte of each word
        exponent' = vrlwmi (unwrds4 0) v 
            (let 
                b = 24
                e = 31  -- 24 to 31 is the last byte where we want the exponent to go
                n = 9   -- rotate the signed bit and exponenet out of the left and into the right
                w = n + (2^(31-15 :: Word32))*b + (2^(31-23 :: Word32))*e
            in 
                (unwrds4 w))

        -- expDiv3shift16 :: VR a
        expDiv3shift16 = approxDiv3 exponent

        -- Put the high two bits of the remainder, verified to be accurate,
        -- into the low-order byte of each word,
        -- and set all other bytes to zero.

        remainder = expDiv3shift16 -- TODO redo this for new exponent position
        -- The remainder effects the (23-bit) mantissa.
        -- Look up precomputed third roots of unity:
        -- Compare the remainder with $0\cdot 64,1\cdot 64,2\cdot 64$ to 
        -- form masks and use them to select 
        -- $2^|remainder|$ from pre-calculated values $2^0, 2^{1/3}, 2^{2/3}$.

        -- oneOrCbrt2 :: VR a
        oneOrCbrt2 = xxsel (unfloats4 1) (root3Of1 1)
                            (vcmpgtub remainder (unbytes16(64))) -- 2^6

        -- cbrtRem :: VR a
        cbrtRem = xxsel oneOrCbrt2 (root3Of1 2)
                        (vcmpgtub remainder (unbytes16(128))) -- 2^7

        root3Of1 k = unfloats4 $ (1 + 2**(-24)) * 2**(k/3)

        -- Combine the byte containing the sign bit with the bytes with the mantissa
        -- of $1, 2^{1/3}, 2^{2/3}$.

        signMant = shufB v cbrtRem 
            [0,17,18,19, 4,21,22,23, 8,25,26,27, 12,29,30,31]

        -- Merge sign (from input) and mantissa (from root of unity) bits with exponent.

        signCbrtExp = xxsel signMant
                (vslw expDiv3shift16 (unwrds4 7))
                -- (equivJoin "Cbrt.signCbrtExp" "unknonw" 
                --         $ map  (\ f -> f expDiv3shift16 (unwrds4 7)) [vslw, vrlq])
                (unwrds4 2139095040) -- 2^31 - 2^23

        -- Merge the mantissa bits with a constant $1.0$ to form $1.$mantissa.

        frac = onePlusMant 23 v

        -- Using either the argument or the fractional bits which have 
        -- been extracted, take the bits with values $2^{22}, 2^{21}, 2^{20}$
        -- and form a lookup key, then use it to look up 
        -- |length expCoeffs24bits| coefficients from register
        -- values constructed using the polynomial coefficients
        -- |expCoeffs24bits|.

        coeffs = lookup8Word (22,20) expCoeffs24bits v

        -- Evaluate a polynomial approximation of $x^{1/3}$ on |frac|$\in[1,2)$.
        evalPoly = hornerV coeffs frac
-}
signMask :: PowerISA repr => repr VR
signMask = unwrds4 0x7fffffff

cbrtDev :: forall repr. PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
cbrtDev v = 
  let
    absv = xxland v signMask
    maybeSubnormal = (xvcmpgtsp absv (unfloats4 $ 1))
    adjustBefore = xxsel (unfloats4 $ 2**(48))  (unfloats4 $ 2**(-48)) maybeSubnormal
    adjustAfter = xxsel (unfloats4 $ 2**(-16))  (unfloats4 $ 2**(16)) maybeSubnormal
    isInfinity = (vcmpgefp absv (unfloats4 $ 2**128-2**104))
    isANumber = vcmpeqfp v v
    vAftermul = xvmulsp adjustBefore absv
    normalResult = xxsel unsigned (unwrds4 0) denormal
    resultSubnormal = xvmulsp adjustAfter normalResult
    resultInfinity = xxsel resultSubnormal (unfloats4 $ 1/0) isInfinity
    resultIsnum     = xxsel (unfloats4 $ 0/0) resultInfinity isANumber    
    result = xxsel v resultIsnum signMask

    denormal = vcmpequw exponentVar (unwrds4 0) --didn't verify dont know exactly what it is 
    unsigned = xvmulsp signCbrtExp evalPoly
    {-
    exponent' = vrlwmi (unwrds4 0) vAftermul
            (let 
                b = 24
                e = 31  -- 24 to 31 is the last byte where we want the exponent to go
                n = 9   -- rotate the signed bit and exponenet out of the left and into the right
                w = n + (2^(31-15 :: Word32))*b + (2^(31-23 :: Word32))*e
            in 
                (unwrds4 w))
    -}
    exponentVar = xxland vAftermul (unwrds4 0x7F800000)

    expDiv3shift16 = approxDiv3 exponentVar
    remainder = xxland expDiv3shift16 (unwrds4 0x00600000)
    oneOrCbrt2 = xxsel (unfloats4 1) (root3Of1 1)
                            (xvcmpgtsp remainder (unwrds4 0)) -- 2^6
    cbrtRem = xxsel oneOrCbrt2 (root3Of1 2)
                        (xvcmpgtsp remainder (unwrds4 0x200000)) -- 2^7
    root3Of1 k = unfloats4 $ 2**(k/3)
    signMant = shufB vAftermul cbrtRem 
            [0,17,18,19, 4,21,22,23, 8,25,26,27, 12,29,30,31]
    signCbrtExp = xxsel signMant
                (expDiv3shift16)
                -- (equivJoin "Cbrt.signCbrtExp" "unknonw" 
                --         $ map  (\ f -> f expDiv3shift16 (unwrds4 7)) [vslw, vrlq])
                (unwrds4 2139095040) -- 2^31 - 2^23
    frac = onePlusMant 23 vAftermul
    coeffs :: [repr VR]
    coeffs = lookup8Word (22,20) expCoeffs24bits vAftermul
    evalPoly = hornerV coeffs frac
  in
    ( if testApproxDiv3 then
         result
      else 
         error "test approxDiv3 failed"
    , [ ("v",v)
      , ("normalResult",normalResult)
      , ("resultSubnormal", resultSubnormal)
      , ("resultInfinity", resultInfinity)
      , ("resultIsnum", resultIsnum)
      , ("result", result)
      ]
    ) 

-- One of the patterns we use only calculates an accurate value
-- under a complicated set of preconditions,
-- so we define the function |approxDiv3| at top level;
-- we used it above at its general type.

-- divShiftMA _p 0 _s _n _v        = error "MathUtils.div by 0"
-- divShiftMA p q s n v = if s /= 0 then vadduwm (vmulosh m v) b else if m' < 1024  && m' > 0 then vmulosh v (unwrds4 m') else if m' < 512  && m' >= -512 then vmulosh v (unwrds4 m') else vmulosh v m
--   where       -- using integer exponent and division
--     m' = (p * 2^n + (q-1)) `div` q
--     m = unwrds4 m'
--     b = unwrds4 $ (s * 2^n) `div` q

-- take 8 bits of biased exponent in normal position (bits 1..8) with bias
-- if exp is the exponent, then the even halfword has (exponent+127) * 2^7 
-- and perform approximate divide by 3, putting the integer part in the same bits
-- doing the approximate multiply, gives us 
--    (exponent+127) * 2^7 / 3  =   (exponent/3 + 127) * 2^7 - 2*127/3 * 2^7
-- and the remainder/fraction in the lower bits
-- the top two bits of the fraction will be
--   0 rem -> 0b01  (0x2 = 0b0010)
--   1 rem -> 0b10  (0x5 = 0b0101)
--   2 rem -> 0b11  (0x7 = 0b0111)
approxDiv3 :: PowerISA repr => repr VR -> repr VR
approxDiv3 v = --divShiftMA 1 3 (fromIntegral $ 2 * expBias) 16 -- where is exponent in v? depends on vrlwmi
    let
        slope, intercept :: Word32
        slope = 21845 * 2^(16 :: Word32) -- 2^16/3 
        intercept = (-214 * 2^(7 :: Word32) - 42) * 2^(16 :: Word32) + 127 * 21845 * 2^(7 :: Word32) * 2^(16 :: Word32) -- approximately 2*127/3 * 2^7
    in
        --vmuleuh v (unwrds4 slope)
        vadduwm (vmuleuh v (unwrds4 slope)) (unwrds4 intercept)

testOne :: Word32 -> [(Word32, String)]
testOne x = map (\(whole,frac)->(whole,Numeric.showHex frac "")) values
  where
    pwrConst :: Word32
    pwrConst = 7 + 16
    values = map (flip divMod (2^pwrConst))
      $ wrds @Interp
      $ approxDiv3 (unwrds4 $ x * 2^(7 :: Word32) * 2^(16 :: Word32))

mapTestOne :: [Word32] -> [Word32]
mapTestOne xs = map (\lst -> fst (head lst)) $ map testOne xs --[0..255]

expectedOutput :: [Word32]
expectedOutput =
  map (\(x, _) -> fromIntegral (x+expBias))
    $ map (\x -> divMod (x-expBias) 3) [0..255]   -- (\(x,y) -> ((x-127),y)) $ divMod (div ((x+127) * (2^7) * 21845 + 11000) (2^16)) (2^7)

testApproxDiv3 :: Bool
testApproxDiv3 = (mapTestOne [0..255]) == (expectedOutput)

-- testApproxDiv3 x = List.all (map testOne x) [-126..127] 
-- approxDiv = map (\x -> (\(x,y) -> ((x-127),y)) $ divMod (div ((x+127) * (2^7) * 21845 + 1422211111) (2^16)) (2^7)) [-126..127] 

expBias :: Word8
expBias = 127 

-- We test that for all input values we are going to use,
-- the precondition that the first two fractional bits
-- in the approximate division by three are correct holds;
-- for this purpose we use |approxDiv3| at type |Val -> Val|,
-- where |Val| is the interpreter type for SPU vectors.

{-}
cbrtAssert :: Bool
cbrtAssert = List.and [divMod i 3 == extractDivMod (approxDiv3 $ bias i) | i <- [expBias - 255 .. expBias]]
    where
        bias i = unwrds4 @Interp $ fromIntegral $ i + expBias
        extractDivMod w = case bytes @Interp w of
            _ : v1 : v2 : _  -> (fromIntegral $ v1 - expBias, fromIntegral $ div v2 64)
            _                -> error "impossible"
-}

-- It would be more efficient to calculate this in an
--  similar way to cube root:  negating the exponent (which only 
--  requires a change of constant, and using polynomials 
--  which approximate $x^{-1/3}$.}

rcbrtSP :: forall repr. PowerISA repr => repr VR -> repr VR
rcbrtSP v = fst $ rcbrtDev v -- recipSP (cbrtSP v)


rcbrtDev :: forall repr. PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
rcbrtDev v =
    let
        intermediate = cbrtSP v
        result = recipSP intermediate
    in
        ( result
        , [ ("v",v)
        , ("intermediate",intermediate)
        , ("result",result)
        ]
        ) 


-- Minimax approximation of cbrtSPU(x) where domain x is divided into $8$ regions $[i/8,(i+1)/8)$ $i=0..8$
-- Order of minimax approximation $= 3$
expCoeffs24bits :: [[Double]]
expCoeffs24bits = 
    [[0.5036005651,0.5226949459,0.5404727498,0.5571349411
    ,0.5728848349,0.5877829689,0.6019692862,0.6155263422]
    ,[0.7121312013,0.6610777392,0.6183207451,0.5819018537
    ,0.5503549260,0.5228162289,0.4984696885,0.4767575813]
    ,[-0.2683807615,-0.2228560464,-0.1885638057,-0.1620208506
    ,-0.1409519673,-0.1239796765,-0.1100488845,-0.09845583825],
    [0.05264905822,0.03911057356,0.02993901534,0.02348843134
    ,0.01879672904,0.01530916074,0.01265158876,0.01058786316]
    ]
