{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Trig(tanSP,tanSPDev,sinSP,sinSPDev,sinSimple,cosSP,cosSimple,cosisinSP,sincosSP,sinFamily,tanFamily, Flags(..)) where

import ISA.PowerISA
import Sqrt
import MathUtils
-- import Data.Word
import Coconut.BaseTypes
import Prelude hiding (sin,cos,tan)



-- Changed functions 
-- shufb -> vperm
-- fma -> xvmaddmsp
-- fs -> xvsubsp
-- fm -> xvmulsp
-- SPU.xor -> vxor
-- fmns -> xvnmsubmsp
-- fa -> xvmaddmsp
-- selb -> xxsel
-- csflt -> vcfsx
-- cflts -> vctsxs

data Flags = Flags {accurateReduction :: Bool, finalNudge :: Bool, isCos :: Bool, subnormalOutput :: Bool, outputNaN :: Bool}

tanSP :: PowerISA repr => repr VR -> repr VR
-- | relatively accurate version on tan
tanSP = tanFamily (Flags { accurateReduction = True,finalNudge = True, isCos = False, subnormalOutput=True, outputNaN=True })
tanSPDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
tanSPDev = tanFamilyFun (Flags { accurateReduction = True,finalNudge = True, isCos = False, subnormalOutput=True, outputNaN=True })

-- -- | absolutely accurate version on tan
-- tan :: PowerISA repr => repr VR -> repr VR
-- tan v = Sqrt.div (fst result) (snd result)
--     where result = sincosSP v

-- | version of sign good for very small numbers
sinSP :: PowerISA repr => repr VR -> repr VR
sinSP = sinFamily (Flags {accurateReduction = True, finalNudge = False, isCos = False, outputNaN = True, subnormalOutput = True})
sinSPDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
sinSPDev = sinFamilyDev (Flags {accurateReduction = True, finalNudge = False, isCos = False, outputNaN = True, subnormalOutput = True})

-- | absolute but not relative accuracy
sinSimple :: PowerISA repr => repr VR -> repr VR
sinSimple v = fst $ sincosSP v

cosSP :: PowerISA repr => repr VR -> repr VR
cosSP = sinFamily (Flags {accurateReduction = True, finalNudge = True, isCos = True, outputNaN = True, subnormalOutput = True})

cosSimple :: PowerISA repr => repr VR -> repr VR
cosSimple v = snd $ sincosSP v

cosisinSP :: PowerISA repr => repr VR -> (repr VR, repr VR)
-- cosisinSP :: (SPUType a,HasJoin String (VR a)) => VR a -> (VR a,VR a)
cosisinSP v =  (vperm sin cos $ unbytes $ [16..19] ++ [0..3] ++ [20..23] ++ [4..7]
             ,vperm sin cos $ unbytes $ [24..27] ++ [8..11] ++ [28..31] ++ [12..15]
             )
  where
    (sin,cos) = sincosSP v


-- | This combined (sin,cos) function is optimized for speed versus error of the complex number, which is absolute error, not relative error.

-- sincosSP :: (SPUType a, HasJoin String (VR a)) => VR a -> (VR a,VR a)
-- Not debugged rn
sincosSP :: (PowerISA repr) => repr VR -> (repr VR, repr VR) 
sincosSP v = (sin, cos) -- (vcfsx (andi fixedPointRot 0xf0) 0, cosRot) 
  -- (sin, cos)
  where
    vBy2PiMinv8 = xvmaddmsp v (unfloats4 $ (1+2**(-24)) * 256*8/(2*pi)) 
                             (unfloats4 $ 0 + 128) 
    -- 24 bits integral | 8 bits fraction 
    fixedPointRot = vctsxs vBy2PiMinv8 0  
    -- 24 bits segments | 8 bits ignored  (bigger numbers have no precision)
    intRot = vperm fixedPointRot fixedPointRot 
             $ unbytes [0,1,2,0x80,4,5,6,0x80,8,9,10,0x80,12,13,14,0x80]
    rotAsFloat = vcfsx intRot 8 
    frac = if False -- True if you don't want better range reduction
           then xvmaddmsp rotAsFloat (unfloats4 $ 0 - pi/4 * (1+1*2**(-24))) 
                    v 
           else xvmaddmsp rotAsFloat (unfloats4 $ ((201/64 - pi/4)* (1+2**(-24))) )
                    (xvmaddmsp rotAsFloat (unfloats4 $ 0 - 201/64 ) 
                     v 
                    )

    frac2 = xvmulsp frac frac 
    (sinCoeffs, cosCoeffs) = splitAt (length (sinShortCoeffs::[[Double]])) 
                             $ lookup8Word (7, 5) (sinShortCoeffs ++ cosShortCoeffs) fixedPointRot

    sinInSeg = xvmulsp frac (hornerV sinCoeffs frac2) 
    cosInSeg = hornerV cosCoeffs frac2 

    [sinRot, cosRot] = case lookup8Word (10, 8) sinCosRot fixedPointRot of
      [a, b] -> [a, b]
      _      -> error "Unexpected result from lookup8Word."


        
    cos = xvmsubmsp cosInSeg cosRot (xvmulsp sinInSeg sinRot) 
    sin = xvmaddmsp cosInSeg sinRot (xvmulsp sinInSeg cosRot) 
      -- want to multiply smaller product first for sensitive case sinRot ~ 0



sinCosRot :: Floating a => [[a]]
sinCosRot = 
           [[ 0, 0.707106781186547524400844362105*(1+1.75*2**(-24))
            , 1, 0.707106781186547524400844362105*(1+1.75*2**(-24))
            , 0, -0.707106781186547524400844362105*(1+1.75*2**(-24))
            , -1,    -0.707106781186547524400844362105*(1+1.75*2**(-24))]
           ,[ 1, 0.707106781186547524400844362105*(1+1.75*2**(-24))
           , 0, -0.707106781186547524400844362105*(1+1.75*2**(-24))
           , -1, -0.707106781186547524400844362105*(1+1.75*2**(-24))
           , 0,    0.707106781186547524400844362105*(1+1.75*2**(-24))]
           ] 

sinShortCoeffs :: Floating b => [[b]]
sinShortCoeffs = map (map (*(1+2**(-24)))) [[0.99999967508991849982913879740673603, 0.99999995674099332353882517651468264, 0.99999999797466108914043695259053865, 1.0, 1.0, 0.99999999797466108914043695259053865, 0.99999995674099332353882517651468264, 0.99999967508991849982913879740673603], [-0.16665821185540832584034525908296639, -0.16666441018134246993509809131718590, -0.16666634752767442224176585435874060, -0.16666666850709332252923262033813042, -0.16666666850709332252923262033813042, -0.16666634752767442224176585435874060, -0.16666441018134246993509809131718590, -0.16665821185540832584034525908296639], [0.82616802102470877150950479606403712e-2, 0.82959251908218795753860463358527404e-2, 0.83188164175898770628490763090502250e-2, 0.83319085364844475078792960714480145e-2, 0.83319085364844475078792960714480145e-2, 0.83188164175898770628490763090502250e-2, 0.82959251908218795753860463358527404e-2, 0.82616802102470877150950479606403712e-2]] 

cosShortCoeffs :: Floating b => [[b]]
cosShortCoeffs = map (map (*(1+2**(-24)))) [[0.99999772895092190672944150242091111, 0.99999969742052948098563433196649010, 0.99999998582705747571159910102746492, 1.0, 1.0, 0.99999998582705747571159910102746492, 0.99999969742052948098563433196649010, 0.99999772895092190672944150242091111], [-0.49994089326852952370393019011252336, -0.49998421546787543186439513472845068, -0.49999776665159274476713034638724271, -0.50000001288305819254427853430215524, -0.50000001288305819254427853430215524, -0.49999776665159274476713034638724271, -0.49998421546787543186439513472845068, -0.49994089326852952370393019011252336], [0.41165584270245007716892118017346320e-1, 0.41404944990178095857000627390475607e-1, 0.41565069561942457775053154371558505e-1, 0.41656693546258180923766542435991728e-1, 0.41656693546258180923766542435991728e-1, 0.41565069561942457775053154371558505e-1, 0.41404944990178095857000627390475607e-1, 0.41165584270245007716892118017346320e-1]] 


-- 2 for lookup on frac, 1 for frac$^2$

-- polyOrd:=2;
-- typeInt:=2;
-- x0 := 0; - Pi / 8;
-- dx := 2*Pi/8/8;
-- pad := 2*Pi/8/256;
-- a1[4]:=numapprox[minimax](x->limit((sin(y)-y)/(y^3),y=sqrt(x))
--                          ,x0-pad..(x0+dx+pad)^typeInt,[polyOrd-1,0],1,'er[0]');
-- aa[4]:=x->1+x*a1[4](x);evalf(log[2](er[0]*1/8));
-- aa[3]:=aa[4];
-- for i from 5 to 7 do
--   aa[i]:=numapprox[minimax](x->simplify(eval(sin(y)/(y),y=sqrt(x)))
--                            ,(x0+dx*(i-4)-pad)^typeInt..(x0+dx*(i-3)+pad)^typeInt,[polyOrd,0],1,'er[i]');
--   aa[7-i]:=aa[i];
-- od;
-- seq(log[2](er[i]*(i+1)/8),i=0..7);

-- plot({-23,seq(log[2](abs(sin(x)-(x*aa[i](x^2)))),i=0..7)},x=-Pi/8..Pi/8);
-- lprint([seq([seq(coeff(simplify(aa[i](x)),x,j),i=0..7)],j=0..polyOrd)]);

-- polyOrd:=2;
-- a1[4]:=numapprox[minimax](x->limit((cos(y)-1)/(y^2),y=sqrt(x))
--                          ,x0-pad..(x0+dx+pad)^typeInt,[polyOrd-1,0],1,'er[0]');
-- aa[4]:=x->1+x*a1[4](x);evalf(log[2](er[0]*1/8));
-- aa[3]:=aa[4];
-- for i from 5 to 7 do
--   aa[i]:=numapprox[minimax](x->simplify(eval(cos(y),y=sqrt(x)))
--                            ,(x0+dx*(i-4)-pad)^typeInt..(x0+dx*(i-3)+pad)^typeInt,[polyOrd,0],1,'er[i]');
--   aa[7-i]:=aa[i];
-- od;
-- seq(log[2](er[i]*(i+1)/8),i=0..8);
-- plot({-23,seq(log[2](abs(cos(x)-(aa[i](x^2)))),i=0..7)},x=-Pi/8..Pi/8);
-- lprint([seq([seq(coeff(simplify(aa[i](x)),x,j),i=0..7)],j=0..polyOrd)]);


-- Accurate sine, with both flags True.
sinFamily :: (PowerISA repr) => Flags -> repr VR -> repr VR
sinFamily flags v = fst $ sinFamilyDev flags v

sinFamilyDev :: PowerISA repr => Flags -> repr VR -> (repr VR, [(String, repr VR)])
sinFamilyDev flags v = 
  let 
    quarterRevs =  if isCos flags 
                    then xvmaddmsp v (unfloats4 (1/pi)) (unfloats4 0.5)
                    else xvmulsp v (unfloats4 (1/pi) ) -- fudge is hard because of signs

    -- isZero = vcmpeqfp quarterRevs (unfloats4 0)

    -- isNegative = xvcmpgtsp (unfloats4 (0)) quarterRevs
    
    -- 31-15 bits - integer multiple of 2 pi
    --  14   bit  - pos/neg multiplier for sine
    --  13   bit  - symmetry bit (used to xor sector bits)
    -- 12-10 bits - 1/8 sectors for lookup
    -- 9-0   bits - fractional bits to improve rouding
    fixedRevs = vctsxs quarterRevs 14

    -- offset by pi/2, and add bit for better rounding
    roundedFixedRevs = vadduwm fixedRevs (unwrds4 $ 2^(13 :: Integer))
    truncatedFixedRevs = vsraw roundedFixedRevs (unwrds4 14)
    intRevs = vcfsx truncatedFixedRevs 0 -- convert to ieee float
    
    frac = (if isCos flags then (flip xvaddsp (unfloats4 $ pi/2)) else id) $
            if accurateReduction flags 
            then xvnmsubmsp piLow intRevs (xvnmsubmsp piMid intRevs (xvnmsubmsp piHigh intRevs v))
            else xvnmsubmsp (unfloats4 $ pi) intRevs quarterRevs
    
    -- set byte 3 to 0xff if bit 13 is set
    reverseSectors = vcmpequw sectorBit (unwrds4 0)
    sectorBit = xxland roundedFixedRevs (unwrds4 (2^(13 :: Integer)))
    segment = vxor roundedFixedRevs reverseSectors
    
    (nudgeInter':nudgeSlope:coeffs, _) = lookup8WordDev (12, 10) (Trig.sinNudgeTable ++ sinSymTable) segment

    
    nudged = if finalNudge flags
                then xvmaddmsp frac evalPoly
                        (xvmaddmsp frac nudgeSlope nudgeInter)
                else xvmulsp frac evalPoly
    evalPoly = (hornerV coeffs (xvmulsp frac frac)) -- fudge these
                    
    nudgeInter = vxor nudgeInter' (signReverseSectors)
    -- I think we could get sign bits without pipe0 instructions
    signReverseSectors = xxland (vperm reverseSectors reverseSectors 
                                (unbytes [2,0x80,0x80,0x80
                                                  ,6,0x80,0x80,0x80
                                                  ,10,0x80,0x80,0x80
                                                  ,14,0x80,0x80,0x80
                                                  ]
                                )
                                )
                                (unwrds4 (2^(7 :: Integer)))
    
    -- signBit' = vrlwmi (unwrds4 0) roundedFixedRevs 
    --                  (let 
    --                       b = 0
    --                       e = 0  -- 0 to 0 is the last byte where we want the exponent to go
    --                       n = (31-14)   -- rotate the signed bit and exponenet out of the left and into the right
    --                       w = n + (2^(31-15 :: Word32))*b + (2^(31-23 :: Word32))*e
    --                   in 
    --                       (unwrds4 w)
    --                  )


                
    beforeSpecialCaseResult = vxor signBit nudged

    isInfinity = vor (xvcmpgtsp quarterRevs (unfloats4 (0x7f7fffff))) (vcmpgefp (xxland v $ unfloats4 (0x7fffffff)) (unfloats4 (0x7fffffff))) -- 0x7f7fffff = biggest 32-bit floating point number

    isANumber = vcmpeqfp v v

    resultInfinity = xxsel beforeSpecialCaseResult (unfloats4 $ 0/0) isInfinity

    result     = if outputNaN flags then 
                        xxsel (unfloats4 $ 0/0) resultInfinity isANumber
                     else 
                        resultInfinity 

  in 
    ( result
    , [ 
         ("v", v)
      ,  ("quarterRevs",quarterRevs)
      , ("fixedRevs",fixedRevs)
      , ("roundedFixedRevs",roundedFixedRevs)
      , ("and 2^14", xxland roundedFixedRevs (unwrds4 $ 2^(14 :: Integer)))
      , ("truncatedFixedRevs",truncatedFixedRevs)
      -- , ("vsraw roundedFixedRevs (unwrds4 14)",vsraw roundedFixedRevs (unwrds4 14))
      -- , ("vsrw roundedFixedRevs (unwrds4 14)",vsrw roundedFixedRevs (unwrds4 14))
      , ("intRevs",intRevs)
      , ("frac", frac)
      , ("sectorBit", sectorBit)
      , ("reverseSectors",reverseSectors)
      , ("segment",segment)
      , ("index",xxland segment (unwrds4 (2^(13 :: Integer)-2^(10 :: Integer))))
      , ("nudged",nudged)
      , ("evalPoly",evalPoly)
      , ("nudgeInter",nudgeInter)
      , ("signReverseSectors",signReverseSectors)
      , ("signBit",signBit)
      , ("result",result)
      ] -- ++ lookupDbg
    )


shortPi :: Double
shortPi =  201/64
midPi :: Double
midPi = 127/131072
piHigh :: (PowerISA repr) => repr VR
piHigh = unfloats4 shortPi --unwrds4 1078530010
-- type signature
piMid :: (PowerISA repr) => repr VR
piMid = unfloats4 midPi
piLow :: (PowerISA repr) => repr VR
piLow = unfloats4 ((pi-midPi-shortPi)*(1+2**(-24))) --unwrds4 874652008

-- polyOrd:=3;
-- aa[0]:=numapprox[minimax](x->limit(sin(y)/(y),y=sqrt(x))
--                          ,-(Pi/2*1/8*(1+2^(-9)))^2..(Pi/2*1/8*(1+2^(-9)))^2,[polyOrd,0],1,'er[0]');
-- evalf(log[2](er[0]*1/8));
-- for i from 1 to 7 do
--   aa[i]:=numapprox[minimax](x->simplify(eval(sin(y)/(y),y=sqrt(x)))
--                            ,(Pi/2*i/8*(1-2^(-9)))^2..(Pi/2*(i+1)/8*(1+2^(-9)))^2,[polyOrd,0],1,'er[i]');
-- od;
-- seq(log[2](er[i]*(i+1)/8),i=0..7);
-- plot({seq(log[2](abs(sin(x)-(x*aa[i](x^2)))),i=0..7)},x=-Pi/2..Pi/2);
-- lprint([seq([seq(coeff(simplify(aa[i](x)),x,j),i=0..7)],j=0..polyOrd)]);

sinSymTable :: [[Double]]
sinSymTable = [[0.99999999999922702510, 0.99999999984370922466, 0.99999999076767071435, 0.99999986501509599529, 0.99999900381814442750, 0.99999511025877038369, 0.99998170877421971966, 0.99994366750418064730], [-0.16666666666663855870, -0.16666665855961670668, -0.16666650692882362578, -0.16666549828691415797, -0.16666148944040601436, -0.16664970997637504271, -0.16662131018869399460, -0.16656176807248594862], [0.83333374613950262182e-2, 0.83331887959117752626e-2, 0.83323258588881543312e-2, 0.83295922659785202237e-2, 0.83233183108989587714e-2, 0.83113704500498490980e-2, 0.82912267763814171616e-2, 0.82600668513016355917e-2], [-0.19841275470417432452e-3, -0.19735024520497867024e-3, -0.19566232434339052787e-3, -0.19315340222804814194e-3, -0.18984965228577948405e-3, -0.18578536220665011015e-3, -0.18100243079896207297e-3, -0.17554975621929884583e-3]]

-- polyOrd:=1;
-- b[0]:=x->2^(-24)*x;
-- for i from 1 to 7 do
--   b[i]:=numapprox[minimax](x->simplify(eval(2^(-24)*sin(y),y=x))
--                           ,(Pi/2*i/8*(1-2^(-9)))..(Pi/2*(i+1)/8*(1+2^(-9))),[polyOrd,0],1,'er[i]');
-- od;
-- seq(log[2](er[i]*(i+1)/8),i=0..7);
-- plot({seq(b[i](x),i=0..7),sin(x)*2^(-24)},x=0..Pi/2,y=0..2^(-24));
-- lprint([seq([seq(coeff(simplify(b[i](x)),x,j),i=0..7)],j=0..polyOrd)]);

sinNudgeTable :: [[Double]]
sinNudgeTable = [[0, 0.489444513593836590588547627400e-9, 0.226983068964788964508763994490e-8, 0.611266076012095226152606193250e-8, 0.126131515542673104721517077463e-7, 0.221532070602178759221433889819e-7, 0.348624605352824339365767732251e-7, 0.505942270300807088248013590016e-7], [1/16777216, 0.569421188639349505982642520396e-7, 0.524751669178859597957392434459e-7, 0.459917600577172460361734038244e-7, 0.377411009902610530057088445156e-7, 0.280403001538692878452340823303e-7, 0.172621876574393799789102157069e-7, 0.582098425822505140818845448618e-8]]

-- Accurate tangent with both flags true.
tanFamily :: PowerISA repr => Flags -> repr VR -> repr VR
tanFamily flags v = fst $ tanFamilyFun flags v
tanFamilyFun :: (PowerISA repr) => Flags -> repr VR -> (repr VR, [(String, repr VR)])
tanFamilyFun flags v =
    let


        quarterRevs = xvmulsp v (unfloats4 (1/(pi)) ) -- fudge is hard because of signs
        
        -- 31-15 bits - integer multiple of 2 pi
        --  14   bit  - pos/neg multiplier for sine
        --  13   bit  - symmetry bit (used to xor sector bits)
        -- 12-10 bits - 1/8 sectors for lookup
        -- 9-0   bits - fractional bits to improve rouding
        fixedRevs = vctsxs quarterRevs 14
        -- offset by pi/2, and add bit for better rounding
        --  a == 32 bit add 
        roundedFixedRevs = vadduwm fixedRevs (unwrds4 $ 2^(13 :: Integer))
        truncatedFixedRevs = vsraw roundedFixedRevs (unwrds4 14)
        intRevs = vcfsx truncatedFixedRevs 0
        
        frac = if accurateReduction flags 
               then xvnmsubmsp piLow intRevs (xvnmsubmsp piMid intRevs (xvnmsubmsp piHigh intRevs v))
               else xvnmsubmsp (unfloats4 $ pi) intRevs quarterRevs
        
        -- set byte 3 to 0xff if bit 13 is set
        reverseSectors = vcmpequw sectorBit (unwrds4 0)
        sectorBit = xxland roundedFixedRevs (unwrds4 (2^(13 :: Integer)))
        segment = vxor roundedFixedRevs reverseSectors
        
        (nudgeInter':nudgeSlope:coeffs) = lookup8Word (12,10) (Trig.tanNudgeTable ++ Trig.tanSymTable) segment
        
        evalPoly = if finalNudge flags 
                   then xvmaddmsp (xvmulsp denom frac) (hornerV coeffs (xvmulsp frac frac)) 
                            (xvmaddmsp frac nudgeSlope nudgeInter)
                   else xvmulsp denom $ xvmulsp frac
                        (hornerV coeffs (xvmulsp frac frac)) -- fudge these
        
        denom = recipSP (xvmaddmsp frac frac (unfloats4 $ (-(pi/2)^(2 :: Integer))))
        
        nudgeInter = vxor nudgeInter' (signReverseSectors)
        -- I think we could get sign bits without pipe0 instructions
        signReverseSectors = xxland (vperm reverseSectors reverseSectors 
                                    (unbytes [2,0x80,0x80,0x80
                                                     ,6,0x80,0x80,0x80
                                                     ,10,0x80,0x80,0x80
                                                     ,14,0x80,0x80,0x80
                                                     ]
                                    )
                                   )
                                   (uninteger (0)) -- ISA only has xxland so put immediate value into a vector?
    in 
    (evalPoly
    , [ 
         ("v", v)
      ,  ("quarterRevs",quarterRevs)
      , ("fixedRevs",fixedRevs)
      , ("roundedFixedRevs",roundedFixedRevs)
      , ("and 2^14", xxland roundedFixedRevs (unwrds4 $ 2^(14 :: Integer)))
      , ("truncatedFixedRevs",truncatedFixedRevs)
      -- , ("vsraw roundedFixedRevs (unwrds4 14)",vsraw roundedFixedRevs (unwrds4 14))
      -- , ("vsrw roundedFixedRevs (unwrds4 14)",vsrw roundedFixedRevs (unwrds4 14))
      , ("intRevs",intRevs)
      , ("frac", frac)
      , ("sectorBit", sectorBit)
      , ("reverseSectors",reverseSectors)
      , ("segment",segment)
      , ("index",xxland segment (unwrds4 (2^(13 :: Integer)-2^(10 :: Integer))))
      , ("nudgeInter",nudgeInter)
      , ("signReverseSectors",signReverseSectors)
      , ("signBit",signBit)
      , ("evalPoly",evalPoly)
      ] -- ++ lookupDbg
    )
-- tanFamily _ _ = error "tanFamily"

tanSymTable :: [[Double]]
tanSymTable = [[-2.46740110027247089325005259267, -2.46740110070440041337809150078, -2.46740112665973544027162920697, -2.46740150600560501302467395178, -2.46740430482559333204624887674, -2.46741824297763507171262985886, -2.46747233167330974993069880801, -2.46764974525889358508554122543], [0.177532966684346838721756634363, 0.177532988958492880696588127868, 0.177533421595253028837219539882, 0.177536455744328542199496062544, 0.177549444239629300554688957476, 0.177591476087637831954522950994, 0.177705713119814219089301832527, 0.177982434554193204570844272127], [0.434650595940409137376045127092e-2, 0.434612181282011296410375672206e-2, 0.434366639981969043195932055052e-2, 0.433546862057581890238855455530e-2, 0.431520713938807021040594017719e-2, 0.427271735840133630771327429238e-2, 0.419196981108128215277009935265e-2, 0.404766829313356657726447668957e-2], [0.172582532001805546257830315986e-3, 0.174917500997260434947296789378e-3, 0.179705611512421335253255176734e-3, 0.187205153348769896564244633606e-3, 0.197838735655172872662287344453e-3, 0.212242889413003235022484014184e-3, 0.231348438382241239934599598481e-3, 0.256509045203223381930932178539e-3]]

-- just repeat the last term, because we can't fix it this way, we probably have to nudge the denominator
tanNudgeTable :: [[Double]]
tanNudgeTable = map (map (*(1+2**(-24)))) [[0, -0.107520242776883182286814642350e-8, -0.579418511880704763591578262070e-8, -0.199386267357749959748449251007e-7, -0.598048741372373151651290987689e-7, -0.187428186378456621728760375477e-6, -0.810790208849357923402717609236e-6, -0.810790208849357923402717609236e-6], [1/16777216, 0.653685422219606240887675324501e-7, 0.771197736435824613363769309039e-7, 0.100788576472579496406027091699e-6, 0.150919880002715488864699614029e-6, 0.279230260286275821232171185262e-6, 0.799400790147669642021330709886e-6, 0.799400790147669642021330709886e-6]]
