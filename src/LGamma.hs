{-# OPTIONS -fwarn-unused-binds -fwarn-unused-matches #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts #-}
module LGamma(lgammaLookup,evenBreaks,offsets1,offsets,lgammaDev,lgammaSP,lgammaKeyResult,lgammaKeyResultDev,lgammaC) where

import ISA.PowerISA
import Coconut.BaseTypes
import MathUtils
import Log      

{-
\subsection{Log Gamma}

we should estimate polynimals until they get to the max single precesion number 

 $maxLGamma:= 10^8$

because of the different behavior of polynomials in different intervals, there are different polynial approximation for each one. we are approximating 
all the with degree $6$.
\begin{maple}
put the maple code for all polynomials here!
\end{maple}

Experiment found sixteen-way lookup with equal intervals to be effective.
-}

lgammaLookup :: RegLookupSpec
lgammaLookup = RegLookupSpec
  { mantissaBits    = 0
  , exponentBits    = 4
  , skipIntervals   = 0
  , rangeEnd        = 10000 
  }

-- \edcomm{CKA}{Try |0.125*(sum [2^i |$\vert$| i<-[0..15]])| and use all 15 intervals.}

{-
With this definition, evaluating
\begin{spec}
evenSPUBreaks lgammaLookup
\end{spec}
calculates the intervals in a form that can easily be transferred to Maple.
this logarithmcally stepped interval are generates by calling the below function with these input arguments:

        RegLookupSpec 0 4 1 10000

\begin{maple}
breaks:=[0.0,0.15259021896696423,0.4577706569008927,1.0681315327687495,2.2888532845044636,4.730296787975891,9.613183794918747,19.378957808804458,38.91050583657588,77.97360189211872,156.0997940032044,312.3521782253758,624.8569466697185,1249.866483558404,2499.885557335775,4999.923704890517,10000.0];

\end{maple}

-}

--Offset from points of interest

evenBreaks :: [Double]
evenBreaks = evenSPBreaks lgammaLookup
offsets1 :: [Double]
offsets1 = [0,0,1,2] ++ (map roundToSP $ zipWith (\ x y -> 0.5 * (x + y)) (drop 4 evenBreaks) (drop 5 evenBreaks))
offsets :: Fractional a => [a]
offsets =  [0,0
           ,1,1
           ,3.4332795143,3.4332795143,3.4332795143  -- breaks 4 - 7
--           ,3.4332795143,3.4332795143,3.4332795143  -- breaks 4 - 7
           ,29.06843655,29.06843655,29.06843655     -- breaks 7 - 10
           ,234.14968872,234.14968872,234.14968872  -- breaks 10 - 13
           ,1874,1874                               -- breaks 13 - 15
           ,7500]


--TODO
-- lgammaSPU :: (SPUType a, HasJoin String (VR a)) => (VR a,VR a) -> (VR a,VR a)
-- lgammaDev :: (PowerISA repr) => (repr VR, repr VR) -> (repr VR, repr VR)
lgammaDev :: PowerISA repr => (repr VR, repr VR) -> ((repr VR, repr VR), [(String, repr VR)])
lgammaDev = use16X2lookupDev lgammaLookup
  lgammaC lgammaKeyResultDev
lgammaSP :: PowerISA repr => (repr VR, repr VR) -> (repr VR, repr VR)
lgammaSP = use16X2lookup lgammaLookup --[[1..16]] lgammaKeyResult 
  lgammaC lgammaKeyResult

-- lgammaKeyResult :: forall repr . (PowerISA repr) => [repr VR] -> repr VR -> (repr VR,repr VR)
lgammaKeyResult :: PowerISA repr => [repr VR] -> repr VR -> (repr VR, repr VR)
lgammaKeyResult coeffs key = fst $ lgammaKeyResultDev coeffs key
lgammaKeyResultDev :: PowerISA repr => [repr VR] -> repr VR -> ((repr VR, repr VR), [(String, repr VR)])
lgammaKeyResultDev coeffs key = 
  let 
    -- Every path through the code uses $\log x$ 
    logx = logeSP key -- :: repr VR
--In addition to common polynomial evaluation, we use selection for four types of intervals, using comparison with branch points: 
    branches = evenSPBreaks lgammaLookup
--Case I:  (intervals 0 and 1) $x < $breaks[3]
-- $a(x) - \log x$
    isI = xvcmpgtsp ( unfloats4 (branches !! 2) ) key -- :: repr VR 
    keyMOffset = xxsel (xvsubsp key offsetIIorIII) key isI
    resultI = xvsubsp polyVal logx
--Case II: (intervals 2 and 3) $x >= $breaks[3] and $x < $breaks[5] 
-- $$a(x) \log x$$
    resultIIorIII = xvmulsp polyVal logxOrMlog2 -- :: repr VR 
-- Case III: (intervals 4 to 14) $x >= $breaks[5] and $x < $breaks[15] 
-- $$a(x) (\log x - \log 2)$$
    isIII = xvcmpgtsp key $ unfloats4 (branches !! 4) -- :: repr VR 
    logxOrMlog2 = xxsel  (xvmulsp logx (xvsubsp key (unfloats4 2))) 
                        (xvsubsp logx $ unfloats4 $ log 2) 
                        isIII -- :: repr VR
    offsetIIorIII = xxsel (unfloats4 $ offsets !! 3) offsetIII isIII
    offsetIII  = xxsel offset4to10 offset10to15 
               $ xvcmpgtsp key $ unfloats4 $ branches !! 10
    offset4to10  = xxsel (unfloats4 $ offsets !! 5) (unfloats4 $ offsets !! 8) 
               $ xvcmpgtsp key $ unfloats4 $ branches !! 7
    offset10to15  = xxsel (unfloats4 $ offsets !! 11) (unfloats4 $ offsets !! 14) 
               $ xvcmpgtsp key $ unfloats4 $ branches !! 13
-- Case IV: (intervals 15) $x >= $breaks[15] 
-- $$(x-1/2) \log x - x + 1/2 \log (2\pi) = x (\log x - 1) - 1/2 \log x + 1/2 \log (2\pi)$$
-- Let $a_0 = 1 - $ulp, $a_1 = - 1/2$, $a_2 =  + 1/2 \log (2\pi)$.
    isIV = xvcmpgtsp key $ unfloats4 (branches !! 15) -- :: repr VR 
    resultIV = xvmaddmsp (xvsubsp key (unfloats4 $ 1/2)) logx (xvsubsp (unfloats4 $ 1/2 * log (2 * pi)) key)
-- we can use isIV to separate the I and IV cases:
    resultIorIIorIII = xxsel resultIIorIII resultI isI
    unexceptional = xxsel resultIorIIorIII resultIV isIV
    result = unexceptional -- TODO figure out if it handles big numbers properly maxFloat (xvcmpgtsp minFloat key)
-- This key is used by |use16X2lookup| to look-up |coeffs|,
-- and also together with these coefficients
-- to evaluate the resulting polynomials using Horner's rule:
    polyVal = hornerV coeffs keyMOffset -- :: repr VR
  in 
    ((key, result),
     [ 
        -- ("coeffs", coeffs)
        ("key", key)
      -- , ("logx", logx)
      -- -- , ("branches", branches)
      -- , ("isI", isI)
      -- , ("keyMOffset", keyMOffset)
      -- , ("resultI", resultI)
      -- , ("resultIIorIII", resultIIorIII)
      -- , ("isIII", isIII)
      -- , ("logxOrMlog2", logxOrMlog2)
      -- , ("offsetIIorIII", offsetIIorIII)
      -- , ("offsetIII", offsetIII)
      -- , ("offset4to10", offset4to10)
      -- , ("offset10to15",offset10to15)
      -- , ("isIV",isIV)
      -- , ("resultIV",resultIV)
      -- , ("resultIorIIorIII",resultIorIIorIII)
      -- , ("unexceptional",unexceptional)
      -- , ("polyVal",polyVal)
      , ("result",result)
      ] -- ++ lookupDbg
    )


-- \begin{maple}
-- Digits:=20;
-- polyOrd := 2; denOrd := 0;
-- \end{maple}

-- \begin{maple}
-- breaks:=[0.0,0.15259021896696423,0.4577706569008927,1.0681315327687495,2.2888532845044636,4.730296787975891,9.613183794918747,19.378957808804458,38.91050583657588,77.97360189211872,156.0997940032044,312.3521782253758,624.8569466697185,1249.866483558404,2499.885557335775,4999.923704890517,10000.0];
-- offsets :=  [0,0
--            ,1,1
--            ,3.4332795143,3.4332795143,3.4332795143  #-- breaks 4 - 7 #           ,3.4332795143,3.4332795143,3.4332795143 #   breaks 4 - 7
--            ,29.06843655,29.06843655,29.06843655     #-- breaks 7 - 10
--            ,234.14968872,234.14968872,234.14968872  #-- breaks 10 - 13
--            ,1874,1874                               #-- breaks 13 - 15
--            ,7500];
-- \end{maple}

-- \begin{maple}
-- for i from 4 to 5 do 
--   f:=(evalf(((lnGAMMA(y)/(log(y)-(log(2)))-(lnGAMMA(offsets[i+1]))/((log(offsets[i+1])-log(2))))/(y-offsets[i+1]))));
--   axp[i] := numapprox[minimax](x-> limit(f
--                                         , y = x+offsets[i+1])
--                               , breaks[i+1]-offsets[i+1] .. breaks[i+2]-offsets[i+1]
--                               , [polyOrd-1, denOrd], 1, 'er'); 
--   log[2](er); 
--   ax[i](x) := lnGAMMA(offsets[i+1])/(log(offsets[i+1])-log(2)) + axp[i](x); 
-- od;
-- \end{maple}
--   plot({ -24, log[2](abs(lnGAMMA(x)-ax[i](x-offsets[i+1])*(log(x)-log(2)))/lnGAMMA(x))}, x = breaks[i+1] .. breaks[i+2]) end do

-- \begin{maple}
-- lprint([seq([seq(coeff(simplify(aa[i](x)),x,j),i=0..15)
--             ]
--            ,j=0..polyOrd)
--        ]);
-- \end{maple}

lgammaC :: [[Double]]
--Original
--                                             a0                                                                       a1                                                                                      a2                                                                              a3                                                                      a4                                                                              a5                                                                      a6                                                              a7                                                                                      a8                                                                      a9                                 a10                                          a11                                         a12                            a13                                      a14                       a15         
lgammaC=  map (map (*(1+2**(-24)))) [[ 0, -0.429008935790033854175401709500e-5,  0.577215666847026043401598761499,     0.577216107574765511591877022392,     2.08766918362530820651957345965,      2.08761339237519271781634425311,    2.08330896116518242683763618289,     25.4507521573146655814944394665,      25.4505481842040538817212634994,      25.4354675891917202819561218125,      218.684422212657823397445502117,       218.683687944079518955982226828,       218.626344942839823962487164556,      1789.54612462147829646858920847,       1789.54298684146200273668493180,         0]
  , [-0.577215664329562631907533906649, -0.577097969702794102629563555866,     0.433571233794554511855367681867e-1,  0.433454750569813938643115544441e-1,  0.874961875230181255558627475536,     0.875111305219250765474913049967,     0.878740461701682841937194965266,     0.925388329117668252077297140591,     0.925459563506415355164067184898,     0.927073271867918973456781123191,      0.948992039339940272551486810367,      0.949024177763531965814131669940,      0.949789259650705509939966554762,      0.961703068254094880012199311554,      0.961720208538455352389605349992,     0]
  , [ 0.822466757759585365328247679741,  0.821101045128000313853990838843,    -0.152918495251845709966328477135e-1, -0.151975538992533352930724158105e-1,  0.476760633320401648741988550309e-2,  0.460007435992569924732612471227e-2,  0.324696745023987524456065825941e-2,  0.271001499484390181334540267382e-3,  0.260579494273100928881395376786e-3,  0.183980436334700688457717966482e-3,   0.170852574911466215129231610381e-4,   0.164964395361302714000464742028e-4,   0.119709753032293562272454016042e-4,   0.123977798329158144312676883024e-5,   0.120060338210045189171934659022e-5,  0]
  , [-0.400664065579621490212732010717, -0.391915616845875791701162623110,     0.693049252463634178481768679383e-2,  0.666326159230372378122761786899e-2, -0.576048263715929129140978120198e-3, -0.471977752489669164253056405913e-3, -0.183130195259079307170925646782e-3, -0.420212738658443695922574711071e-5, -0.336035013702633629831506952308e-5, -0.127194031230166884836432901034e-5,  -0.313251060907539340268111075123e-7,  -0.253789426604260489051127651529e-7,  -0.100070620549622316752784747293e-7,  -0.271434775718397056271136048246e-9,  -0.222101543939068380605600064747e-9,  0]
  , [ 0.269959559038001080338126570314,  0.236357632177902334342678952473,    -0.483614884849428013069046907590e-2, -0.282451073048022531817170300481e-2,  0.864015174391098581663324273200e-4,  0.451961705815409971896448321145e-4,  0.787117352493656894349109006607e-5,  0.854958257775215798045256842557e-7,  0.413714613582819644891643383353e-7,  0.682643007497908857761030426009e-8,   0.771258002180896346989900476044e-10,  0.383176951044300721785157733716e-10,  0.662747304792876875887937354509e-11,  0.812673460868094659935673831556e-13,  0.411932190365900751605304462250e-13, 0]
  , [-0.199170916414064098456093810466, -0.123082357963119559428095799860,    -0.125411017091316319764423625201e-2,  0.882202049506524311517157602406e-3, -0.147588202040743319444374096052e-4, -0.295251911192968872017964762649e-5, -0.206128111037490505179648042815e-6, -0.224219655888182761747450652004e-8, -0.348040869057077481755977263049e-9, -0.224268788142529189926469224401e-10, -0.247084634852268805497485551340e-12, -0.399622709234077499170733427757e-13, -0.270478958512542419388441085207e-14, -0.317733639859401665660373023578e-16, -0.532308897299627310700911347344e-17, 0]
  , [ 0.117435733969572972630877099241,  0.357921666227196124462377819246e-1, -0.550827027256992240357855122352e-2, -0.136141315237240784464702479860e-3,  0.220918089117525742340315365847e-5,  0.909900954415091720951233577812e-7,  0.240771224439973298712472671265e-8,  0.580667986178242835283261490501e-10, 0.137740891914835485666640674394e-11, 0.329114753863296920841109312375e-13,  0.798335218888540489916424794817e-15,  0.197006424806004523746685851455e-16,  0.494498241273087949826060372118e-18,  0.126122273228514472081995803025e-19,  0.326425085381568062844293986720e-21, 0]]

{--Original
--                                             a0                                                                       a1                                                                                      a2                                                                              a3                                                                      a4                                                                              a5                                                                      a6                                                              a7                                                                                      a8                                                                      a9                                 a10                                          a11                                         a12                            a13                                      a14                       a15         
--lgammaC=  map (map (*(1+2**(-24)))) [[ 0                               , -0.429008935790033854175401709500e-5,  0.577215666847026043401598761499,     0.577216107574765511591877022392,     2.08766918362530820651957345965,      2.08761339237519271781634425311,    2.08330896116518242683763618289,     25.4507521573146655814944394665,      25.4505481842040538817212634994,      25.4354675891917202819561218125,      218.684422212657823397445502117,       218.683687944079518955982226828,       218.626344942839823962487164556,      1789.54612462147829646858920847,       1789.54298684146200273668493180,         0]
--                                                                 , [-0.577215664329562631907533906649, -0.577097969702794102629563555866,     0.433571233794554511855367681867e-1,  0.433454750569813938643115544441e-1,  0.874961875230181255558627475536,     0.875111305219250765474913049967,     0.878740461701682841937194965266,     0.925388329117668252077297140591,     0.925459563506415355164067184898,     0.927073271867918973456781123191,      0.948992039339940272551486810367,      0.949024177763531965814131669940,      0.949789259650705509939966554762,      0.961703068254094880012199311554,      0.961720208538455352389605349992,     0]
--                                                                 , [ 0.822466757759585365328247679741,  0.821101045128000313853990838843,    -0.152918495251845709966328477135e-1, -0.151975538992533352930724158105e-1,  0.476760633320401648741988550309e-2,  0.460007435992569924732612471227e-2,  0.324696745023987524456065825941e-2,  0.271001499484390181334540267382e-3,  0.260579494273100928881395376786e-3,  0.183980436334700688457717966482e-3,   0.170852574911466215129231610381e-4,   0.164964395361302714000464742028e-4,   0.119709753032293562272454016042e-4,   0.123977798329158144312676883024e-5,   0.120060338210045189171934659022e-5,  0]
--                                                                 , [-0.400664065579621490212732010717, -0.391915616845875791701162623110,     0.693049252463634178481768679383e-2,  0.666326159230372378122761786899e-2, -0.576048263715929129140978120198e-3, -0.471977752489669164253056405913e-3, -0.183130195259079307170925646782e-3, -0.420212738658443695922574711071e-5, -0.336035013702633629831506952308e-5, -0.127194031230166884836432901034e-5,  -0.313251060907539340268111075123e-7,  -0.253789426604260489051127651529e-7,  -0.100070620549622316752784747293e-7,  -0.271434775718397056271136048246e-9,  -0.222101543939068380605600064747e-9,  0]
--                                                                 , [ 0.269959559038001080338126570314,  0.236357632177902334342678952473,    -0.483614884849428013069046907590e-2, -0.282451073048022531817170300481e-2,  0.864015174391098581663324273200e-4,  0.451961705815409971896448321145e-4,  0.787117352493656894349109006607e-5,  0.854958257775215798045256842557e-7,  0.413714613582819644891643383353e-7,  0.682643007497908857761030426009e-8,   0.771258002180896346989900476044e-10,  0.383176951044300721785157733716e-10,  0.662747304792876875887937354509e-11,  0.812673460868094659935673831556e-13,  0.411932190365900751605304462250e-13, 0]
--                                                                 , [-0.199170916414064098456093810466, -0.123082357963119559428095799860,    -0.125411017091316319764423625201e-2,  0.882202049506524311517157602406e-3, -0.147588202040743319444374096052e-4, -0.295251911192968872017964762649e-5, -0.206128111037490505179648042815e-6, -0.224219655888182761747450652004e-8, -0.348040869057077481755977263049e-9, -0.224268788142529189926469224401e-10, -0.247084634852268805497485551340e-12, -0.399622709234077499170733427757e-13, -0.270478958512542419388441085207e-14, -0.317733639859401665660373023578e-16, -0.532308897299627310700911347344e-17, 0]
--                                                                 , [ 0.117435733969572972630877099241,  0.357921666227196124462377819246e-1, -0.550827027256992240357855122352e-2, -0.136141315237240784464702479860e-3,  0.220918089117525742340315365847e-5,  0.909900954415091720951233577812e-7,  0.240771224439973298712472671265e-8,  0.580667986178242835283261490501e-10, 0.137740891914835485666640674394e-11, 0.329114753863296920841109312375e-13,  0.798335218888540489916424794817e-15,  0.197006424806004523746685851455e-16,  0.494498241273087949826060372118e-18,  0.126122273228514472081995803025e-19,  0.326425085381568062844293986720e-21, 0]]
-}


