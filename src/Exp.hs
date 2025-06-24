{-# LANGUAGE NoMonomorphismRestriction #-}
module Exp where

import Sqrt
import ISA.PowerISA
import Coconut.Core.CoreISA

import MathUtils
import Coconut.BaseTypes


expSP :: PowerISA repr => repr VR -> repr VR
expSP = expFamily (Flags { lowRange=True, highRange=True, base=BaseE, subnormalOutput=True, outputNaN=True, offset=Offset0 }) expCoeffsWithRounding
expSPDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
expSPDev = expFamilyDev (Flags { lowRange=True, highRange=True, base=BaseE, subnormalOutput=True, outputNaN=True, offset=Offset0 }) expCoeffsWithRounding

exp2SP :: PowerISA repr => repr VR -> repr VR
exp2SP = expFamily (Flags { lowRange=True, highRange=True, base=Base2, subnormalOutput=True, outputNaN=True, offset=Offset0 }) expCoeffsWithRounding
exp2SPDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
exp2SPDev = expFamilyDev (Flags { lowRange=True, highRange=True, base=Base2, subnormalOutput=True, outputNaN=True, offset=Offset0 }) expCoeffsWithRounding
-- calculates the exp.
-- the slowest but correct and default version of exp, this is the third version of exp.

data Base = Base2 | BaseE | Base10 deriving (Eq,Show)
data Offset = Offset0 | Offset1 deriving (Eq,Show)

data Flags = Flags { lowRange :: Bool, highRange :: Bool, base :: Base, subnormalOutput :: Bool, outputNaN :: Bool, offset :: Offset }

-- expFamily :: (PowerISA repr) => Flags -> [[Double]] -> repr VR -> repr VR
expFamily :: PowerISA repr => Flags -> [[Double]] -> repr VR -> repr VR
expFamily flags coeffLists v = fst $ expFamilyDev flags coeffLists v

-- expFamilyDev :: (PowerISA repr) => Flags -> [[Double]] -> repr VR -> (repr VR, [(String, repr VR)])
expFamilyDev :: PowerISA repr => Flags -> [[Double]] -> repr VR -> (repr VR, [(String, repr VR)])
expFamilyDev flags coeffLists v =
  let 
    vBylog2 = ( if offset flags == Offset0
                  then 
                    case base flags of
                      Base2 -> v
                      BaseE -> xvmulsp v (unfloats4 (1/log(2)))
                      Base10 -> xvmulsp v (unfloats4 (1/log(10)))
                  else 
                    case base flags of
                      Base2 -> xvaddsp v (unfloats4 (-1))
                      BaseE -> xvmaddmsp v (unfloats4 (1/log(2))) (unfloats4 (-1))
                      Base10 -> xvmaddmsp v (unfloats4 (1/log(10))) (unfloats4 (-1))
              )
    -- if we want subnormal outputs, we need to make them normal outputs
    -- and then divide by a power of two
    maybeSubnormal = xvcmpgtsp v (unfloats4 $ -32)
    -- adjustment is +64 if the output is close to subnormal, otherwise 0
    adjustBefore = xxsel (unfloats4 $ 64)      (unfloats4 0) maybeSubnormal
    adjustAfter  = xxsel (unfloats4 $ 2**(-64)) (unfloats4 1) maybeSubnormal
    -- apply adjustment if flag says we care about subnormal outputs
    vByLog2ForSub = if subnormalOutput flags then 
                       xvaddsp adjustBefore vBylog2
                    else 
                       vBylog2
                                  
    -- $\exp x = 2^(x/log 2)$ here we are calculating $x/(log 2)$
    -- The calculated domain of the function is  $-127 < x/\log 2 < 129-1$ulp,
    -- otherwise the function will saturate to $-127$ or to $129-1$ulp according to the sign.
    isZero = xvcmpgtsp (unfloats4 (-160)) vBylog2 
    isInfinity = xvcmpgtsp vBylog2 (unfloats4 (129-128*2**(-23)))
    -- return true unless it is a NaN
    isANumber = vcmpeqfp vBylog2 vBylog2
    -- Convert the floating point argument to an integer and multiply it by $2^23$ to put all $23$ bits of the fractional part in the mantissa. 
    vBylog2AsInt = vctsxs vByLog2ForSub (23)
    -- Use the fractional part and exponent part separately to get $2^(x/log 2)$.
    -- The exponent is masked out, and then unbiased (by adding $127$ in the correct bitfield position).
    expnt = xxland vBylog2AsInt (unwrds4 0xff800000)
    exp2 = vadduwm expnt (unwrds4 0x3f800000)
    -- The fractional part is $1+$position in interval.
    -- Where the intervals are divided by $\{i/8\vert i=0,...,8\}$.
    -- The first three bits of the mantissa are not used for the fractional part, they are used for the lookup.
    frac = onePlusMant 20 vBylog2AsInt
    (coeffs, lookupDbg) = lookup8WordDev (22, 20) coeffLists vBylog2AsInt
    evalPoly = hornerV coeffs frac
    normalResult = xvmulsp exp2 evalPoly
    resultSubnormal = if subnormalOutput flags then 
                        xvmulsp adjustAfter normalResult
                      else 
                        normalResult
    resultInfinity = if highRange flags then 
                        xxsel resultSubnormal (unfloats4 $ 1/0) isInfinity
                     else 
                        resultSubnormal
    resultZero = if lowRange flags then 
                        xxsel resultInfinity (unfloats4 $ 0) isZero
                     else 
                        resultInfinity
    result     = if outputNaN flags then 
                        xxsel (unfloats4 $ 0/0) resultZero isANumber
                     else 
                        resultZero
  in
    ( result
    , [ 
        ("v",v)
      , ("evalPoly",evalPoly)
      , ("frac",frac)
      , ("exp",exp2)
      , ("exponent",expnt)
      , ("vBylog2",vBylog2)
      , ("vBylog2AsInt",vBylog2AsInt)
      , ("vByLog2ForSub", vByLog2ForSub)
      , ("maybeSubnormal", maybeSubnormal)
      , ("adjustBefore", adjustBefore)
      , ("adjustAfter", adjustAfter)
      , ("isInfinity", isInfinity)
      , ("isZero", isZero)
      , ("isANumber", isANumber)
      , ("normalResult", normalResult)
      , ("resultSubnormal", resultSubnormal)
      , ("result",result)
      ] 
     ++ lookupDbg
        -- ++ zipWith (\v index -> ("c" ++ show index, v)) coeffs [0..]
        -- ++ lookupDbg
    )
    

{- The function $\exp(x) - 1$ has a zero at zero, 
so the above calculation will not capture the low-order bits near zero.
We use a different minimax polynomial near zero, and 
use |xxsel| to choose the best approximation for each input.
Maple code to find minimax polynomial for narrow range:
\begin{maple}
lim:=5/8;
breaks:=seq(-lim+2*lim/8*i,i=0..8);
polyOrd:=4;
for i from 0 to 2 do 
  a[ from 5 to 7 do 
  a[i] := numapprox[minimax](x->evalf(exp(x)-1),(breaks[i+1])..(breaks[i+2]),[polyOrd,0],1,'er[i]');log[2](er[i]);
od;
lprint(seq(log[2](er[i]),i=0..7));
lprint([seq([seq(coeff(simplify(a[i](x)),x,j),i=0..7)
            ]
           ,j=0..polyOrd)
       ]);
\end{maple}i] := numapprox[minimax](x->evalf(exp(x)-1),(breaks[i+1])..(breaks[i+2]),[polyOrd,0],1,'er[i]');log[2](er[i]);
od;
i:=3; ax[i] := numapprox[minimax](x->limit(((exp(y)-1)/y-1)/y,y=x),(breaks[i+1])..(breaks[i+2]),[polyOrd-2,0],1,'er[i]');log[2](er[i]);
a[i]:= x -> x + x^2 * ax[i](x);
i:=4; ax[i] := numapprox[minimax](x->limit(((exp(y)-1)/y-1)/y,y=x),(breaks[i+1])..(breaks[i+2]),[polyOrd-2,0],1,'er[i]');log[2](er[i]);
a[i]:= x -> x + x^2 * ax[i](x);
for i -}

m1cutoff :: Fractional a => a
m1cutoff = 5/8 
narrowCoeffs :: Floating b => [[b]]
narrowCoeffs = map (map (*(1+2**(-24))))  [[-0.252469821725069906167513190350e-3, -0.520491563459166653297323796500e-4, -0.418732403016156590358334792010e-5, 0, 0, 0.621611451091939636483829780400e-5, 0.100063269392309723232230828820e-3, 0.629200470710682233647571145860e-3], [0.997622404289896770543727677307, 0.999310762598777099728519871614, 0.999904491608398911684035641523, 1, 1, 0.999860415796846235754079533083, 0.998709048183034074556702143686, 0.994287118247889724517591252110], [0.490979482924618314616903357152, 0.496343087543343660896006840069, 0.499138328082687153996354018064, 0.499999056703636309302690122648, 0.500001046859231795077301091322, 0.501229977873517721773019846999, 0.506586389696737627769661710782, 0.520518203731354410015770928273], [0.149266013780579728544791690494, 0.156882702477195503645618372614, 0.162806471738955820508340820024, 0.166557052743416616479392780102, 0.166547112053291387524527087300, 0.161369580163283251032086245936, 0.150159359535994801227059246313, 0.130542089478598422378891732847], [0.241234036501599230719946779443e-1, 0.282031161917947200305281743819e-1, 0.329727833792890333861326335913e-1, 0.397734951286117063349309072972e-1, 0.436828211654634978646189757379e-1, 0.526903578123117410222496054961e-1, 0.616012692536451099393402749698e-1, 0.720191801918908370934294875327e-1]]

-- Combine narrow and wide approximations, 
-- taking care to include the endpoints in the wide approximation.

expm1SP :: PowerISA repr => repr VR -> repr VR
expm1SP v = xxsel wide narrow $ xvcmpgtsp (unfloats4 m1cutoff) (unfloats $ map abs $ floats v)
  where
    wide = xvsubsp (expSP v) (unfloats4 1)
    -- Break the narrow region up into $8$ segments.  
    -- Multiply by a constan $1$ ulp less than desired to prevent bad rounding on the endpoints.
    key = xvmaddmsp v (unfloats4 $ 1 / (m1cutoff * 4) * (1 - 2**(-25))) (unfloats4 $ 1 + 1 / (4))
    -- Use the key to look up the polynomial coefficients, for the polynomial approximation:
    narrowPoly = lookup8Word (21,19) narrowCoeffs key
    narrow = hornerV narrowPoly v

-- expCoeffs24bits =  map (map (*(1+1*2**(-24))))
--   [[0.48891044350695485758, 0.53316061922513474670, 0.58141577801760612158, 0.63403840182178655684,

--     0.69142377999338053530, 0.75400297863142253485, 0.82224607865020398485, 0.89666570692173328523],

--     [0.38699665481517968710, 0.42202284459154137020, 0.46021917538846515929, 0.50187256948194926125,

--     0.54729591783264606172, 0.59683043045263231840, 0.65084819949852790587, 0.70975499434440434852],

--     [0.066123238148862889030, 0.072107902510200950180, 0.078634225273645770070,

--     0.085751230713052959530, 0.093512380178148102350, 0.10197597368420169445, 0.11120558784869257362,

--     0.12127055346458543413], [0.057969644369562528236, 0.063216345444863077065,

--     0.068937913538461603074, 0.075177327787501201973, 0.081981457273380400124,

--     0.089401413091788440470, 0.097492932287795729211, 0.10631679654005143476]
--  ]


-- This Maple code computes and packages the coefficients for the polynomials:
-- \begin{verbatim}
-- Digits:=30;
-- polyOrd:=4;
-- for i from 0 to 7 do aa[i]:=numapprox[minimax](x->evalf((1+2**(-24))*exp(1*log(2)*(x-1+i/8))), 1..1+(1)/8,[polyOrd,0],1,'d2[i]');od;
-- lprint(seq(log[2](d2[i]),i=0..7));
-- \end{verbatim}
-- with relative bit accuracies
-- \[
-- -25.6373654674945446935652039611, -25.5123654674945446935647287177, -25.387365467\
-- 4945446935645138412, -25.2623654674945446935650246605, -25.1373654674945446935642\
-- 821601, -25.0123654674945446935640240326, -24.8873654674945446935647996939, -24.7\
-- 623654674945446935644505634
-- \]

-- This Maple code plots the functions and log relative errors:
-- \begin{verbatim}
-- interface(plotdevice=maplet);
-- plot({asinSPU(x),seq(aa[i](x),i=0..3),seq(-c1[i+1]-sqrt(aa[i](x)),i=4..7)},x=0..1,y=0..Pi/2);
-- plot({seq(log[2](abs(-asinSPU(x)+ax[i](x))/asinSPU(x)),i=0..3),seq(log[2](abs(-asinSPU(x)-c1[i+1]-sqrt(asqrt[i](x)))/asinSPU(x)),i=4..7)},x=0..1,y=-45..0);
-- \end{verbatim}
-- array for $a$
-- \begin{verbatim}
-- lprint([seq([seq(coeff(simplify(aa[i](x)),x,j),i=0..7)
--             ]
--            ,j=0..polyOrd)
--        ]);
-- \end{verbatim}

expCoeffsWithRounding :: Fractional a => [[a]]
expCoeffsWithRounding = [[0.488910472505717946544333995964, 0.533160650848510271418706237762,

    0.581415812503142018228284047676, 0.634038439428529967623811116225,

    0.691423821003824519148965598205, 0.754003023353629632384100467241,

    0.822246127420115970730423861595, 0.896665760105699212437748621431], [

    0.386996678257891908889515995259, 0.422022870155999920962990896473,

    0.460219203266703906733335040636, 0.501872599883384618334631701530,

    0.547295950985647815642341785321, 0.596830466606234787233071184109,

    0.650848238924312874042930509462, 0.709755037338528327856751565010], [

    0.0661232417618669572168933408550, 0.0721079064502102126340139082810,

    0.0786342295702572469581413173390, 0.0857512353985405885343425226400,

    0.0935123852877072794478736750250, 0.101975979256217663333955160213,

    0.111205593925017272195985227523, 0.121270560090863936668552711023], [

    0.0579696479196762013464425152008, 0.0632163493162893647847828263023,

    0.0689379177602816264169415242142, 0.0751773323914287139050205811200,

    0.0819814622939993592123900582478, 0.0894014185668115541282306153820,

    0.0974929382583513368093151815639, 0.106316803050988670258058888136]]
