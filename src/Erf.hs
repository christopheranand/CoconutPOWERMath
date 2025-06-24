{-# LANGUAGE NoMonomorphismRestriction #-}
module Erf(erfLookup,erfcLookup,evaluateErfLookup,erfKeyResult,erfSP,erfcSP,erfcSPDev,erfOffset,erfCoeff,erfcOffset,erfcOffsets,denErfc,numErfc,erfcKeyResult,erfcKeyResultDev) where

import ISA.PowerISA
import Sqrt       
import Exp       
import MathUtils
import Coconut.BaseTypes

------------- 
--We only need to evaluate it in the interval up to $\erf^{ -1}(1-\ulp)<3.84$.
-- maple
-- maxErf:=fsolve(erf(x)=1-2^(-24), x);

-- Experiment found sixteen-way lookup with equal intervals to be effective.
erfLookup :: RegLookupSpec
erfLookup = RegLookupSpec
    { mantissaBits    = 4
    , exponentBits    = 0
    , skipIntervals   = 14
    , rangeEnd        = 3.832506857 
    }

{-Erfc has a slow drop to zero, so we have to use a different cutoff:
fsolve(2^(-129)=erfc(x),x);
gives 9.30630095746611335475482214642 which we can use to define intervals-}
erfcLookup :: RegLookupSpec
erfcLookup = RegLookupSpec
  { mantissaBits    = 4
  , exponentBits    = 0
  , skipIntervals   = 15
  , rangeEnd        = 9.30630095746611335475482214642 
  }

--Use ghci to evaluate, create variable name personally
--spec
evaluateErfLookup :: [Double]
evaluateErfLookup = {-MathUtils.-}evenSPBreaks erfLookup

--fcmgt,selb,xvnmsubmsp?(fnms)
erfKeyResult :: PowerISA repr => Bool -> [repr Coconut.BaseTypes.VR] -> repr Coconut.BaseTypes.VR -> (repr Coconut.BaseTypes.VR, repr Coconut.BaseTypes.VR)
erfKeyResult _reverse [] _ = error "erfKeyResult _ []"
erfKeyResult rev (offsetVal:coeffs) v = (key, result)
        where
            key = vandc v signBit 
            polyVal = hornerV coeffs (xvsubsp key offsetVal)
            isBig = xvcmpgtsp (unfloats $ map abs $ floats key) (unfloats4 3.832506857)
            resultOrOne = xxsel polyVal (unfloats4 1) isBig
            erf = xxsel resultOrOne v signBit
            result = if  rev 
                     then xvnmsubmsp (unwrds4 0x3F800001) erf (unfloats4 1)
                     else erf



--- erfOffset:erfCoeff, 
erfSP :: PowerISA repr => (repr VR, repr VR) -> (repr VR, repr VR)
erfSP = use16X2lookup erfLookup (erfOffset:erfCoeff) $ erfKeyResult False
erfcSP :: PowerISA repr => (repr VR, repr VR) -> (repr VR, repr VR)
erfcSP = use16X2lookup erfLookup (erfOffset:erfCoeff) $ erfcKeyResult 
erfcSPDev :: PowerISA repr => (repr VR, repr VR) -> ((repr VR, repr VR), [(String, repr VR)])
erfcSPDev = use16X2lookupDev erfLookup (erfOffset:erfCoeff) $ erfcKeyResultDev








erfOffset :: Fractional a => [a]
erfOffset =  [0.0, 0.1277502179145813, 0.2555004358291626, 0.5110008716583252
             , 0.7665013670921326, 1.0220017433166504, 1.277502179145813, 1.5330027341842651
             , 1.7885031700134277, 2.044003486633301, 2.299504041671753, 2.555004358291626
             , 2.810504913330078, 3.0660054683685303, 3.3215057849884033, 3.5770063400268555]

erfCoeff :: Floating b => [[b]]
erfCoeff=  map (map (*(1+2**(-23)))) 
 [[0, 0.143370323660607305221303639043, 0.282148865695395570322226039343, 0.530113889692790114103172998292
    , 0.721633707495521930160053288207, 0.851634425057662314452065651169, 0.929184830649775502651406308497
    , 0.969840798751582805773702597665, 0.988571799005172594783579873637, 0.996155631315956020726296608943
    , 0.998853999075532932398359410589, 0.999697701146043917639447808116, 0.999929518529935890816426685268
    , 0.999985489262449164610113130840, 0.999997364021897898670942561602, 0.999999577770387474731687866314]
    , [1.12837916641300718361712889949, 1.11011331122868893970190939227, 1.05706937798955357572472735023
    , 0.869061620695651582627721848864, 0.627041457358935424359345630922, 0.397045742187578435868880472504
    , 0.220639336516990190236250258221, 0.107602595379226701458128402993, 0.460531619940123304693178365608e-1
    , 0.172978514356412095714626569670e-1, 0.570189389004058122760516517300e-2, 0.164945315677702521517531591536e-2
    , 0.418747617565529355775289474617e-3, 0.932943261655680595349205560960e-4, 0.182409644704011658170364076253e-4
    , 0.312987620188965012842032713292e-5], [0.248374479175994255625664090000e-6, -0.141814277312602724561893809395
    , -0.270008289671335036146873848331, -0.444025377452801251383650947364, -0.480612070103385446860761003251
    , -0.405808294547705201225356135167, -0.281903878607243095571588613836, -0.164977624843865312549976025098
    , -0.823716392993037235021471217646e-1, -0.353532509660365424520122748158e-1, -0.131064881055104122597009572626e-1
    , -0.421100388977857802186775485431e-2, -0.117530109194426852188929830943e-2, -0.285450774618344360797610548824e-3
    , -0.604093641956049582377918237179e-4, -0.111509241541661049090435592362e-4], [-0.376140680642145451343997264808
    , -0.358061998878508379661710199497, -0.307644721926656331846289096254, -0.139563093113730413849322564542
    , 0.362810583479878452119571697836e-1, 0.144570116789148276516082353124, 0.167144603460419859817388888573
    , 0.133116773672583322779730871444, 0.829588172356912156716488912282e-1, 0.423534564984187289305087947871e-1
    , 0.181107401844707225458466394772e-1, 0.656848554973677091292940476260e-2, 0.203659884387559915209522247643e-2
    , 0.542712745274614074657758258883e-3, 0.124767612977369082956353773883e-3, 0.248149779181527041736312144124e-4]
    , [0.274660300302993564244846267500e-3, 0.717048968883273373422030390062e-1, 0.139187762502963410243131912296
    , 0.192584798830134500870936080253, 0.148881666225705077147598205596, 0.584231306164922325114726446934e-1
    , -0.172269615691538357407057697077e-1, -0.498887909669741572335658286876e-1, -0.475037136479700091873973788602e-1
    , -0.311392376749120423449122005379e-1, -0.158791866327562143989034178451e-1, -0.659391041217009363195856413000e-2
    , -0.228106748744436759055264499084e-2, -0.666141605193968342679059040551e-3, -0.165624071273848738942605670879e-3
    , -0.352653713967212800948903258153e-4], [0.111012565977363425099934623351, 0.931743686996461024360236297468e-1
    , 0.438277017181379565369350256262e-1, -0.299031089026938054941107730831e-1, -0.676340980543656381928040624664e-1
    , -0.586702452232240786025080856823e-1, -0.267235343166217542727177736904e-1, 0.578073714360388892346865612336e-3
    , 0.121630472168372552710258288489e-1, 0.118615300375500212782774945508e-1, 0.740348488443464254614039953243e-2
    , 0.350928677775703888980543794575e-2, 0.133800709994110621573778928781e-2, 0.421734488287652933155418802198e-3
    , 0.111612989044893271931779969383e-3, 0.250475075973383081136300176361e-4]]

erfcOffset :: Fractional a => [a]
erfcOffset = [0.0, 0.1277502179145813, 0.2555004358291626, 0.6387511193752289, 0.8942515552043915, 1.1497519612312317
    , 1.40525245666503905, 1.6607529520988464, 1.91625332832336435, 2.171753764152527, 2.4272541999816895, 2.682754635810852
    , 2.93825519084930415, 3.1937556266784668, 3.4492560625076294, 3.704756498336792]

erfcOffsets :: [Double]
erfcOffsets = map roundToSP $ 0:[(((evenSPBreaks erfcLookup) !! i) + ((evenSPBreaks erfcLookup) !! (i+1)))/2 | i<-[1..15]]

denErfc :: PowerISA repr => [repr VR]
denErfc = map unfloats4 [57.3243549817501711376731656681, 419.432148301229528827616007407
                        , 676.624304208496970683097088269, 174.486425352823299374028771495]

numErfc :: PowerISA repr => [repr VR]
numErfc = map unfloats4 [-32.8104515218666557366672994169, -268.730362303391315625378156812
                        , -561.169988532242604772715563406, -264.191275879698123339953131674]

erfcKeyResult :: PowerISA repr => [repr VR] -> repr VR -> (repr VR, repr VR)
erfcKeyResult [] _ = error "erfcKeyResult: Empty list"
erfcKeyResult (coeffOffset:coeffs) v = fst $ erfcKeyResultDev (coeffOffset:coeffs) v
                        
                        

erfcKeyResultDev :: PowerISA repr => [repr VR] -> repr VR -> ((repr VR, repr VR), [(String, repr VR)])
erfcKeyResultDev [] _ = error "erfcKeyResult []"
erfcKeyResultDev (coeffOffset:coeffs) v = 
    let 
        key = vandc v signBit --vc
        polyVal = hornerV coeffs (xvsubsp key coeffOffset) --vc
        isSmallNeg = xvcmpgtsp (unfloats $ map abs $ floats key) (unfloats4 3.832506857) --v
        resultNegOrTwo = xxsel resultNeg (unfloats4 2) isSmallNeg --v
        isNegative = vsraw v (unwrds4 31) --unwrds []-- vsrw v (unwrds4 31) --the max shift is 31, original = rotmai v (-32)
        resultNeg = xvaddsp (unfloats4 1) polyVal --v
        resultPos = xvsubsp (unfloats4 1) polyVal --v
        resultNonAsymp = xxsel resultPos resultNegOrTwo isNegative --v
        result = xxsel resultNonAsymp resultAsymp (xvcmpgtsp v (unfloats4 2.5)) --v
        resultAsymp = xvmulsp vInv (expSP expArg) --v
        vInv = recipSP v --v
        invV2 = xvmulsp vInv vInv --v
        numPoly = hornerV numErfc invV2 --v
        denPoly = hornerV denErfc invV2 --v
        expArg = xvmsubmsp numPoly (recipSP denPoly) (xvmulsp v v) --v
    in 
        ((key, result),
     [ 
        ("v", v),
        ("key", key),
        -- ("polyVal", polyVal),
        -- ("isSmallNeg", isSmallNeg),
        -- ("unfloats4 3.832506857", unfloats4 3.832506857),
        -- ("resultNegOrTwo", resultNegOrTwo),
        -- ("isNegative", isNegative),
        -- ("resultNeg", resultNeg),
        -- ("resultPos", resultPos),
        -- ("resultNonAsymp", resultNonAsymp),
        -- ("resultAsymp", resultAsymp),
        -- ("vInv", vInv),
        -- ("invV2", invV2),
        -- ("numPoly", numPoly),
        -- ("denPoly", denPoly),
        -- ("expArg", expArg),
        ("result", result)
     ])
        

{-helper functions-}
--xvcmpgtsp ra rb = xvcmpgtsp (unfloats $ map abs $ floats ra) (unfloats $ map abs $ floats rb)
