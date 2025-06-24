{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Hyperbolic(coshSP,coshSPDev,fcoshAlt,fcoshAltDev,tanhSaturate,tanhTreshold,tanhLookup,tanhSP,tanhDbg,tanhKeyResult,tanhKeyResultDbg,tanhKeyResultDev,tanhC,sinhSP,sinhSPDev,sinDevFun,sinhAndExpC) where

import Coconut.BaseTypes

import ISA.PowerISA
import MathUtils
import Exp
import Sqrt

-- Hyperbolic cosine is defined by
-- \begin{equation}
-- \cosh(x) = \frac{e^x+e^{-x}}{2} = e^{x-\log 2} + e^{-x-\log 2}.
-- \end{equation}


{- Since hyperbolic cosine is an even function,
we can use the absolute value of the input.  
This has two advantages:
We only need to check for the positive bound on the domain of $\exp$
which maps to non-extreme SPU values,
and we can make the asymmetry in the range of SPU values work for us.
Since the positive exponent range for SPU values is larger than the negative exponent range,
there are representable numbers whose reciprocals are denormals.
Since the SPU doesn't treat denormals properly,
these numbers are treated as zero.
If we want to save time by only calculating $e^x$ \textit{or} $e^{ -x}$, 
we will get the wrong answer if one is representable and one is not,
and we calculate the non-representable one, which is treated as zero.

By only treating the case $x \ge 0$, we know $e^x \ge 1$,
so this doesn't occur. -}

coshSP :: PowerISA repr => repr VR -> repr VR
coshSP v = fst $ coshSPDev v
coshSPDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
coshSPDev v = 
  let
    vPositive = vandc v signBit
    coshInRange = xvmaddmsp (unfloats4 $ 1/4) invExp2v halfExpV

    -- The only way to handle numbers which should saturate
    -- is to treat them as special cases.
    -- In this case,
    -- we form a compare mask
    -- and select either the computed result or the maximum float.
    
    -- TODO figure out if it handles big numbers properly isReallyBig = xvcmpgtsp vPositive (unfloats $ map (abs . acosh) $ floats @Interp $ maxFloat)
    
--     To save an operation, we calculate
--     \begin{equation}
--     \cosh x
--   =
--     \left( \frac{e^x}{2} \right) + \frac14 \left( \frac{e^x}{2} \right)^{-1}
--     \enskip.
--     \end{equation}
    resultOrMax = coshInRange -- TODO figure out if it handles big numbers properly maxFloat isReallyBig
    halfExpV = expFamily (Flags { lowRange=False, highRange=False, base=BaseE, subnormalOutput=True, outputNaN=True, offset=Offset1 }) expCoeffsWithRounding vPositive --third false added for base 2 keep in mind when debugging
    invExp2v = recipSP halfExpV

    isInfinity = xvcmpgtsp vPositive (unfloats4 (0x7f7fffff)) -- 0x7f7fffff = biggest 32-bit floating point number
    isANumber = vcmpeqfp v v

    resultInfinity =  xxsel resultOrMax (unfloats4 $ 1/0) isInfinity
    resultNaN = xxsel (unfloats4 $ 0/0) resultInfinity isANumber

    final = resultNaN
  in
    ( final
    , [ 
        ("v",v)
      , ("resultOrMax",resultOrMax)
      , ("final",final)
      ]
    )

-- This version is more accurate for not huge values.  
-- \edcomm{CKA}{If we can't make $\exp$ more accurate,
-- we might have to understand better why.}
fcoshAlt :: PowerISA repr => repr VR -> repr VR
fcoshAlt v = fst $ fcoshAltDev v
fcoshAltDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
fcoshAltDev v =
    let
        vMinusLog2 = xvsubsp v (unfloats4 $ log 2)
        minusVLog2 = xvmsubmsp v (unfloats4 $ (-1)) 
                           (unfloats4 $ log 2)
        result = xvaddsp (expSP vMinusLog2) (expSP minusVLog2)
    in
    ( result
    , [ 
        ("v",v)
      , ("result",result)
      ]
    )


-- Hyperbolic tangent, see figure~\ref{fig:tanh},
-- is defined by
-- \begin{equation}
-- \tanh(x) = \frac{ e^x-e^{-x} }{ e^x+e^{-x} },
-- \end{equation}
-- but using this definition for computation would be difficult because 
-- we would run into problems with subtraction of similar numbers,
-- and division of similar large and small numbers,
-- all of which introduce additional error.
-- Fortunately, hyperbolic tangent rises very quickly to $1$.\footnote{$1 - 2^{ -25}$ is the smallest number which when rounded to a representable number is $1$.}


tanhSaturate :: Double
tanhSaturate = atanh (1 - 2 ** (-25))   -- |= 9.010913339828708|

tanhTreshold :: Double
tanhTreshold = tanhSaturate + 2 ** (-12)

{-We should have a |roundToRepresentable| in |tanhTreshold|.
Therefore, any number larger than |tanhForget| in magnitude
should round to $\pm1$,
for reasons of numerical stability we use a slightly larger value
to delimit the range  $[-|tanhTreshold|, |tanhTreshold|]$
where we then use polynomial approximations.

Therefore, any number larger than |tanhTreshold| in magnitude
should round to $\pm1$,
and inside the range  $[-|tanhTreshold|, |tanhTreshold|]$
we then use polynomial approximations.

The standard Remez Exchange algorithm \cite{GolubGH-SmithLB-1971_Alg414Approx}
can find best approximating polynomials to a differentiable function on an interval, 
but in this case a single approximating polynomial will either
have very high degree or be very inaccurate.
A standard approach is to look for a piecewise polynomial 
approximation,
and we have standard tools to help in the search.
Unfortunately, the search space is large,
and some domain knowledge is required to narrow the search.
We use a Maple library function to perform the search for a given polynomial.
The Maple procedure can fail for several reasons,
including ill conditioning within the algorithm.
Even if the search succeeds,
the reported quality (maximum error) of the solution
may not meet the requirements for a particular function.
Increasing precision requires either higher-order 
polynomial approximations, 
or a different strategy for assigning intervals.
This has to be decided by the domain expert on a case-by-case
basis,
after a lot of experimentation.

Another problem is that the Maple function will report the 
error assuming infinite precision arithmetic,
so allowances have to be made for the actual precision.
An expert will recognize polynomials which are likely
to introduce unacceptable rounding errors (e.g. the coefficients
grow quickly relative to the size of the domain of the polynomial).
Finally, the reported error is an absolute error.
Since floating point numbers, by definition,
have a level of precision inversely proportional to the
number represented, this is usually not an acceptable measure.
So weighting of the approximation process may need to be added
by hand.
Even this, however, will not lead to satisfactory answers
where the function being approximated crosses zero,
or has a singularity or asymptote.
In such cases, the domain expert will use different strategies
to identify appropriate piecewise polynomial approximations
fitting into interval organisations that allow efficient coding.

%{{{ Our example has no singularities ... tanhLookup
Our example has no singularities or vertical asymptotes,
but it does have a zero,
and this affects the quality of polynomial approximations of a given order.
We can get around this by varying the size of the intervals.
In general,
this will greatly increase the complexity of the interval identification,
but we have identified one pattern which is very efficient:
logarithmically stepped intervals, 
as shown by alternating colours in figure~\ref{fig:tanh}. -}

{- The efficient implementation of this approach relies on the fact that
such logarithmically stepped interval patterns can be specified
by using combinations of bits from both mantissa and exponent
of the floating-point representation as interval identifiers;
different such combinations are possible,
and to specify such an interval pattern
we furthermore need the length of the range to be covered by all intervals,
and we allow to specify an offset into the resulting break-point list.
For $\tanh$, we choose the following interval pattern specification: -}

tanhLookup :: RegLookupSpec
tanhLookup = RegLookupSpec
  { mantissaBits    = 2
  , exponentBits    = 2
  , skipIntervals   = 3
  , rangeEnd        = tanhTreshold
  }

-- With this definition, evaluating lookupBreaks tanhLookup calculates the intervals in a form that can easily be transferred to Maple.

{-In Maple, we used the following code to find approximations,
with the interval break points stored in the array \texttt{breaks}:
\begin{verbatim}
i:=0;                                  # line 1

ax:=numapprox[minimax]                 #      2
      (x->limit((tanh(y)/y-1)/y, y=x)  #      3
      ,(breaks[i+1]) .. (breaks[i+2])  #      4
      ,[polyOrd-2,0]                   #      5
      ,x->x                            #      6
      ,'da[i]');                       #      7
      
aa[0]:=x->x*(1+x*ax(x));               #      8

for i from 1 to 15 do                  #      9      
  aa[i]:=numapprox[minimax]            #     10
      (x->tanh(x)                      #     11
      ,(breaks[i+1]) .. (breaks[i+2])  #     12
      ,[polyOrd,0]                     #     13
      ,x->x                            #     14
      ,'da[i]');                       #     15
od;
\end{verbatim}
The first polynomial is calculated in lines $2$--$7$
separately from the rest (lines $9$--$15$)
because it contains a zero crossing.
The arguments to the approximation generation function
are the function to be approximated (line $3$),
the domain interval of the approximation (line $4$),
the order of the rational polynomial (numerator,denominator) (line $5$),
the weighting (identity) (line $6$)
and the variable in which to store the estimate for the maximum error
(line $7$).

Building on our accumulating experience with variations on these Maple patterns
we are considering automatic generation of such Maple code blocks,
but up to now have not yet seen a real need for this.
%% we 
%% We have not yet seen the need to generate building blocks for such Maple

\paragraph{Example Literate Code:}

Implementing $\tanh$ with the above interval pattern,
which uses four-bit interval identifies,
requires lookup in 16-element arrays;
with the large register file of the SPU,
we can easily afford to store these $6*16$ floating-point numbers
in 24 registers.

This 16-way register lookup
can be performed for two keys at a time without performance loss,
so we use a two-way parallel ``urolling'' of the $\tanh$ function.
The lookup implementation relies on the specification |tanhLookup| shown above,
and uses the coefficient array |tanhC| produced using Maple.
-}
-- tanhSP :: (PowerISA repr) => (repr VR, repr VR) -> (repr VR, repr VR)
tanhSP :: PowerISA repr => (repr VR, repr VR) -> (repr VR, repr VR)
tanhSP = use16X2lookup tanhLookup tanhC tanhKeyResult

tanhDbg :: PowerISA repr => (repr VR, repr VR) -> ((repr VR, repr VR), [(String, repr VR)])
tanhDbg = use16X2lookupDev tanhLookup tanhC tanhKeyResultDbg


  --(use16X2lookup tanhLookup tanhC (fst (tanhKeyResultDev x)), snd $ tanhKeyResultDev x tanhC)

{- The higher-order function |use16X2lookup| implements this
pattern,
generates vector constants containing byte-scrambled versions of the binary
representation of the Maple-generated floats |tanhC|
arranged to ease the lookup,
constructs the 16-way lookup,
and connects it with two instances of |tanhKeyResult|,
%and expects as third argument
a function that accepts as arguments
an original argument |v| (one of the two parallel arguments)
and the coefficients |coeffs| resulting from the lookup,
and produces as its results
a |key| to be used for the lookup
and the final result of the calculation.
This kind of ``tying the knot''
is only possible in non-strict (lazy) programming languages ---
the |key| of course must not depend on the |coeffs|. -}
-- tanhKeyResult :: ([repr VR] -> arg -> (repr VR, result))
tanhKeyResult :: PowerISA repr => [repr VR] -> repr VR -> (repr VR, repr VR)
tanhKeyResult coeffs v = fst $ tanhKeyResultDev coeffs v
tanhKeyResultDbg :: PowerISA repr => [repr VR] -> repr VR -> ((repr VR, repr VR), [(String, repr VR)])
tanhKeyResultDbg coeffs v = tanhKeyResultDev coeffs v
tanhKeyResultDev :: PowerISA repr => [repr VR] -> repr VR -> ((repr VR, repr VR), [(String, repr VR)])
tanhKeyResultDev coeffs v = 
    let
        -- We will use the absolute value as lookup |key| and
        -- as starting point for the remaining computations,
        -- and obtain it by masking out the bit pattern |signBit|
        -- covering exactly the sign bit (of each vector element):\restorecolumns
        key = vandc v signBit 
        -- This key is used by |use16X2lookup| to look-up |coeffs|,
        -- and also together with these coefficients
        -- to evaluate the resulting polynomials using Horner's rule:\restorecolumns
        polyVal = hornerV coeffs key
        -- We also compare
        -- (using the floating-point ``greater-than'' comparison instruction |fcmgt|)
        -- the |key| to $\atanhSPU(1-2^{ -24})$,
        -- because this is the smallest number 
        -- which rounds to $1$ at 24-bit precision
        -- ---
        -- all higher numbers round to $1$.
        -- This comparison produces a select mask:\restorecolumns
        isBig = xvcmpgtsp (key) (unfloats4 (tanhTreshold))
        -- This mask can be used by the bit-select instruction |xxsel|;
        -- we apply this to the result of polynomial evaluation,
        -- to force evaluations at large numbers to $1$.\restorecolumns
        resultOrOne = xxsel polyVal (unfloats4 1) isBig
        -- Finally, we obtain the |result|
        -- by restoring the sign bit from the argument |v|:\restorecolumns
        result = xxsel resultOrOne v signBit
    in
    ( (key, result)
    , [ 
        ("v",v)
      , ("FINAL",result)
      ]
    )

{-
*Hyperbolic SPUTypeIndices> let (a,b) = tanhSPU (unfloats [0.1, 0.2, 0.3, 0.4] , unfloats [1..4] :: SPUSimValue REG) in putStrLn $ unlines $ map show $ floats a ++ floats b
*Hyperbolic> let (a,b) = tanhSPU (unfloats [0.1, 0.2, 0.3, 0.4] , unfloats [1..4] :: VR INTERP) in putStrLn $ unlines $ map show $ floats a ++ floats b
9.9667988717556e-2
0.1973753124475479
0.2913126051425934
0.3799489438533783
0.7615941762924194
0.9640276432037354
0.995055079460144
0.9993292093276978
*Hyperbolic SPUTypeIndices> putStrLn $ unlines $ map (show . tanh) $ [0.1, 0.2, 0.3, 0.4] ++ [1..4 :: Float]                                                    9.9667996e-2
0.19737533
0.29131263
0.37994897
0.7615942
0.9640276
0.9950548
0.9993293
*Hyperbolic> putStrLn $ unlines $ map (show . tanh) $ [0.1, 0.2, 0.3, 0.4] ++ [1..4]
9.966799462495582e-2
0.197375320224904
0.2913126124515909
0.3799489622552249
0.7615941559557649
0.9640275800758169
0.9950547536867305
0.999329299739067
-}
{-
These break points are then copied into the Maple code to compute and package 
the coefficients for the polynomials:
\begin{maple}
Digits:=20;
polyOrd:=5;
breaks :=[0.0,8.251809523809525e-2,0.24755428571428573,0.41259047619047623,0.5776266666666667,0.7426628571428572,1.0727352380952382,1.402807619047619,1.7328800000000002,2.0629523809523813,2.723097142857143,3.383241904761905,4.043386666666667,4.703531428571429,6.023820952380953,7.344110476190477,8.6644]; # pasted from tanhLookup
\end{maple}
%}}}
}%fullcode
\begin{maple}
i:=0;ax:=numapprox[minimax](x->limit((tanh(y)/y-1)/y,y=x),(breaks[i+1])..(breaks[i+2])
                           ,[polyOrd-2,0],x->x,'da[i]');
aa[0]:=x->x*(1+x*ax(x));
for i from 1 to 15 do 
  aa[i]:=numapprox[minimax](x->tanh(x),(breaks[i+1])..(breaks[i+2]),[polyOrd,0],x->x,'da[i]');
od;
\end{maple}
\fullcode{
%{{{ {maple}
\begin{maple}
lprint(seq(log[2](da[i]),i=0..15));
\end{maple}
with relative bit accuracies
\[
-33.068919370445594153, -33.478336062773519316, -32.014124227304172204, -31.910374659934050408, -33.094443818704294005, -26.791078722956517515, -26.306310360922224295, -27.486002313078405576, -30.666552664600894771, -23.732415562795583028, -24.498378211994315797, -25.949981166683440771, -27.574091219835091340, -24.062323530769916939, -27.546829550273871427, -31.092864018218164217
\]
\begin{maple}
plotsetup(ps,plotoptions=`color=rgb,width=7in,height=3.5in,portrait,font=TIMES,noborder`,plotoutput="/Users/anand/coconut/trunk/doc/pictures/plots/tanhSegments.ps");
plot([seq([x,aa[i](x),x=breaks[i+1]..breaks[i+2]],i=0..15)],x=0..8.665,y=0..1.1,colour=[gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue ],labels=["",""]);

plotsetup(ps,plotoptions=`color=rgb,width=7in,height=3.5in,portrait,font=TIMES,noborder`,plotoutput="/Users/anand/coconut/trunk/doc/pictures/plots/tanhBits.ps");
plot([-24,seq(log[2](abs(-tanh(x)+aa[i](x))),i=0..15)],x=0..8.665,y=-45..0,colour=[red,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue ],numpoints=400,labels=["",""]);
\end{maple}
array for $a$
\begin{maple}
lprint([seq([seq(coeff(simplify(aa[i](x)),x,j),i=0..15)
            ]
           ,j=0..polyOrd)
       ]);
\end{maple}
-}

tanhC :: [[Double]]
tanhC = map (map (*(1+2**(-24)))) 
  [[ 0, 0.76665994206216280500e-6, 0.85224457796967820100e-4, 0.87604833396299519900e-3
   , 0.28826976708277484860e-2 , -0.29518187406922070000e-4, -0.52548473181355297416e-1, -0.15522627706521489055
   , -0.22239134093317094828, -0.14818849830438870267, 0.16962735932401807353, 0.50061284130915047403
   , 0.73315721161510582380, 0.90599283167689476495, 0.98193756447065666786, 0.99704558623059203271]
  ,[1.0000000000000000000, 0.99996943220122966545, 0.99842684658403564874, 0.98910225178586482789
   , 0.97164975929233731315, 0.98794878932291692395, 1.2276463233849177634, 1.5959393284662978803
   , 1.7944619559262109691, 1.6283602504387305868, 1.0441377185211133906, 0.55020073310401392379
   , 0.25956975908114499251, 0.77562045514007841719e-1, 0.12308809416021441085e-1, 0.17117868599503903267e-2]
  ,[0.39413492569101083000e-7, 0.48822734742023465500e-3, 0.11926294432086825300e-1, 0.56269829665971465720e-1
   , 0.11726410363654944449, 0.81457018656378044230e-1, -0.35818912003219997721, -0.88826311187155691465
   , -1.1233686347629458232, -0.97469673964311786833, -0.54369382704452863711, -0.24820088523151460615
   , -0.10268943184509330043, -0.25854024067245215551e-1, -0.33759144611288370068e-2, -0.39836788157270384420e-3]
  ,[-0.33333761802143631516, -0.33727332454228743558, -0.38044694111536935300, -0.48680624756023030534
   , -0.59387525810251736940, -0.55556585433954383640, -0.15041088643583571809, 0.23223945309488146972
   , 0.37167830792136534047, 0.30516961673606515074, 0.14565912865814258329, 0.57080245966582064929e-1
   , 0.20598982071566680631e-1, 0.43453300828072904184e-2, 0.46542205946650681241e-3, 0.46523974431250103437e-4]
  ,[0.13687444908134812100e-3, 0.16533891871564607777e-1, 0.99722946433182319344e-1, 0.22842354223621093748
   , 0.32281843644751800314, 0.30310959565080258502, 0.11549279547150333818, -0.23044634482614571336e-1
   , -0.64458234194044679965e-1, -0.49588652424497652065e-1, -0.19974968837264363893e-1, -0.66703983451136292334e-2
   , -0.20906026740668462037e-2, -0.36775600441142730616e-3, -0.32230692405142647334e-4, -0.27254531903066559988e-5]
  ,[0.13186672674077612911, 0.10275748120698747815, 0.37159273402948339438e-1, -0.25700408501329898546e-1
   , -0.59133811649824462091e-1, -0.55325838178198268734e-1, -0.20397687302583290361e-1, -0.27484462738996079627e-3
   , 0.46523402896566972530e-2, 0.33232725411160366992e-2, 0.11170153743486545371e-2, 0.31602927726153386243e-3
   , 0.85726041158383423798e-4, 0.12524179070985273807e-4, 0.89635875262732355492e-6, 0.64046604888916979754e-7]]


{-
Hyperbolic sine is defined by 
\begin{equation}                      \label{sinhDef}
\sinh x = \frac{e^x-e^{ -x}}{2}.
\end{equation}
It is difficult to approximate by polynomials over large ranges, 
because it grows exponentially.
Therfore, for large values we use \eqref{sinhDef},
but for small values of $x$, such that $e^x$ and $e^{ -x}$ are close in value: 
\item[(i)] precision loss grows as $n$ where $x=2^{ -n}$, because of similarity, and
\item[(ii)] errors in $e^x$ have the opposite sign as errors in $e^{ -x}$, 
so they reinforce each other.
The only way to get accurate results near $0$ is to use polynomial approximations.
The Taylor series for $\sinh$ at $0$ is odd, near $0$. 
Odd polynomials with the same number of coefficients are better
at approximating $\sinh$ than general polynomials.
In branchless implementations,
the easiest way to use different methods to calculate $\sinh$ differently
on two intervals is to calculate both approximations, 
and use \texttt{xxsel} to select the correct result.
Since both methods involve polynomials approximations, 
over subintervals, we can try to share the execution of polynomial execution
and/or coefficient lookup.
Calculating a key to look up in multiple intervals efficiently depends on the
structure of the problem.
This is easier to do with the hyperbolic sine intervals near zero,
because the exponential intervals repeat periodically.
The following Maple code calculates minimax polynomials for $\sinh(\sqrt{x})/\sqrt{x}$,
and uses them to construct odd approximating polynomials for $\sinh(x)$
over the four intervals $[0,1)$, $[1,\sqrt{2})$, $[\sqrt2,\sqrt3)$, and $[\sqrt3,2)$.
\fullcode{
\begin{maple}
Digits:=25;
\end{maple}
}
\begin{maple}
polyOrd:=4;
i:=0;
ax:=numapprox[minimax](x->limit((sinh(y)/y-1)/y^2,y=sqrt(x)),0..1,[polyOrd-2,0],x->x,'d2[i]');
aa[i]:=x*(1+x*ax(x));plt[i]:=x*(1+x^2*ax(x^2));
for i from 1 to 3 do
  ax:=numapprox[minimax](x->limit(((sinh(y))/y),y=sqrt(x)),i..i+1,[polyOrd-1,0],x->x,'d2[i]');
  aa[i]:=x*(ax(x));plt[i]:=x*(ax(x^2));
od;
\end{maple}
%}}}

%{{{ Note that the first coefficient
Note that the first coefficient of the first polynomial is forced to be $1$.
This is necessary to ensure relative accuracy for numbers near zero,
which comprise half the representable floating point numbers.
These intervals are chosen for three reasons:
\begin{enumerate}
  \item the minimum ratio between the positive and negative terms in \eqref{sinhDef} 
        is $\log_2(e^2/e^{ -2})=5.77$,
        so approximation errors in the second term must be greater than $32$ ulps 
        before they can effect the result;
  \item the minimax polynomials are calculated using $\sqrt{x}$, 
        so accuracy of the approximations could be expected to depend on the width of the
        intervals measured in this coordinate;
  \item a single computation can produce both two bits to choose the interval 
        and one bit to select between the two calculation types.
\end{enumerate}
The resulting polynomials
\begin{align}
\label{eq:sinhP0}
p_0 &= x \left( 1+{x}^{2} \left(  0.166667+ \left(  0.008330+ 0.000203{x}^{2}
 \right) {x}^{2} \right)  \right) \\
p_1 &= x \left(  0.999986+ \left(  0.166705+ \left(  0.008293+
 0.000215{x}^{2} \right) {x}^{2} \right) {x}^{2} \right) \\
p_2 &= x \left(  0.999884+ \left(  0.166853+ \left(  0.008221+
 0.000227{x}^{2} \right) {x}^{2} \right) {x}^{2} \right) \\
\label{eq:sinhP3}
p_3 &= x \left(  0.999535+ \left(  0.167197+ \left(  0.008107+
 0.000240{x}^{2} \right) {x}^{2} \right) {x}^{2} \right) 
\end{align}
all have better than $23$ bits of relative accuracy,
as shown in \figref{sinh}.
%}}}

%{{{ We calculate $e^x=2^{x/\log 2}$ by separating
We calculate $e^x=2^{x/\log 2}$ by separating the integral part of 
$x/\log 2$ and placing it in the exponent bit field, 
using the two most significant fractional bits to look up 
an interval and the remaining bits to evaluate a polynomial approximation.
The following Maple code calculates minimax polynomials for the final step:
\begin{maple}
for i from 0 to 3 do 
  axx:=numapprox[minimax](x->evalf((2^(x+i/4))),1..1+(1)/4,[polyOrd,0],1,'d2[i+4]');
  aa[i+4]:=axx(x);plt[i+4]:=axx(x);
od;                     
\end{maple}
\begin{align}
\label{eq:sinhP4}
p_4 &= 1.00456+ \left(  0.673227+ \left(  0.274550+ \left( 
 0.026672+ 0.020986x \right) x \right) x \right) x\\
p_5 &=  1.194632+ \left(  0.800607+ \left(  0.326497+ \left( 
 0.031719+ 0.024957x \right) x \right) x \right) x\\
p_6 &=  1.42066+ \left(  0.952087+ \left(  0.388272+ \left( 
 0.037721+ 0.029679x \right) x \right) x \right) x\\
\label{eq:sinhP7}
p_7 &=  1.68946+ \left(  1.13222+ \left(  0.461736+ \left( 
 0.044858+ 0.035294x \right) x \right) x \right) x.
\end{align}
This time using arbitrary polynomials \edcomm{WK}{Not a sentence}.
In infinite precision, the four polynomials are similar,
as seen in \figref{sinh},
but in finite precision they may have different rounding.
This is something we want to take advantage of in a later stage to improve accuracy.
%}}}

%{{{ To share computation for all of these paths
To share computation for all of these paths,
we need to explicitly add $0$ to the first four polynomials so they all
have the same number of coefficients.  
We also need to switch the variable in the polynomial evaluation between $x$ and $x^2$. 
The switches between one version for the interval $[0,2)$ and another for
the outer interval could be made with \texttt{xxsel},
but we can also use \texttt{vperm} instructions in the odd pipeline 
for better balance.
%}}}

\fullcode{
%{{{ Maple
sinh part

\begin{maple}
plotsetup(ps,plotoptions=`color=rgb,width=7in,height=3.5in,portrait,font=TIMES,noborder`,plotoutput="/Users/anand/coconut/trunk/doc/pictures/plots/sinhBits.ps");
plot([-24,seq(log[2](abs(-sinh(x)+plt[i])/sinh(x)),i=0..3)],x=0..2,y=-35..0,colour=[red,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue ],numpoints=400,labels=["",""]);

plotsetup(ps,plotoptions=`color=rgb,width=7in,height=3.5in,portrait,font=TIMES,noborder`,plotoutput="/Users/anand/coconut/trunk/doc/pictures/plots/sinhSegments.ps");
plot([seq([x,plt[i],x=sqrt(i)..sqrt(i+1)],i=0..3)],x=0..2,colour=[gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue ],numpoints=400,labels=["",""]);
\end{maple}

exp part

\begin{maple}
plotsetup(ps,plotoptions=`color=rgb,width=7in,height=3.5in,portrait,font=TIMES,noborder`,plotoutput="/Users/anand/coconut/trunk/doc/pictures/plots/sinhExpSegments.ps");
plot([seq([x,subs(x=x-i/4,plt[i+4]),x=1+i/4..1+(i+1)/4],i=0..3)],x=1..2,colour=[gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue ],labels=["",""]);

plotsetup(ps,plotoptions=`color=rgb,width=7in,height=3.5in,portrait,font=TIMES,noborder`,plotoutput="/Users/anand/coconut/trunk/doc/pictures/plots/sinhExpBits.ps");
plot([-24,seq(log[2](abs((2^(x)-subs(x=x-i/4,plt[i+4]))/2^(x))),i=0..3)],x=1..2,y=-35..0,colour=[red,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue,gold,blue ],numpoints=400,labels=["",""]);
\end{maple}
\fullcode{
%{{{ Maple
\begin{maple}
lprint(seq(log[2](d2[i]),i=0..7));
\end{maple}
with relative bit accuracies
\begin{maple}
\[ -25.21321174893333858682884, -24.82702831326115873229095
 , -23.99862667778597795287062, -23.44191119046707179817610
 , -27.42519595186189836421832, -27.17519595186189849173306
 , -26.92519595186189857098841, -26.67519595186189836894852\]

seq(latex(plt[i]),i=0..7);
\end{maple}

\begin{maple}
lprint([seq([seq(coeff(simplify(aa[i]),x,j),i=0..7)
            ]
           ,j=0..polyOrd)
       ]);
\end{maple}
%}}}
}%fullcode

We now go through the implementation to explain how the keys to select 
execution paths are generated unconditionally.
-}

-- Still doesn't work with values over 2
sinhSP :: PowerISA repr => repr VR -> repr VR
sinhSP v = fst $ sinhSPDev v
sinhSPDev :: PowerISA repr => repr VR -> (repr VR, [(String, repr VR)])
sinhSPDev v = sinDevFun (Flags { lowRange=False, highRange=False, base=Base2, subnormalOutput=False, outputNaN=False, offset=Offset0 }) v
sinDevFun :: PowerISA repr => p -> repr VR -> (repr VR, [(String, repr VR)])
sinDevFun _ v =
  let
    -- Evaluate on the absolute value, so truncation always goes the same way,\restorecolumns
    vPositive = vandc v signBit 

    -- isInfinity = xvcmpgtsp v (unfloats4 (0x7f7fffff)) -- 0x7f7fffff = biggest 32-bit floating point number
    -- isInfinity = (xvcmpgtsp v (unfloats4 (0x7f7fffff)))
    -- isNegativeInfinity = (vcmpgefp (xxland v $ unfloats4 (0x7fffffff)) (unfloats4 (0x7fffffff)))

    -- since $\sinh$ the signed result is obtained by copying the sign from the input.\restorecolumns
    signedResult = xxsel resultOrMax v signBit
    -- Detect inputs which will saturate on the output and generate a mask\restorecolumns which needs to be to saturate results before the sign is copied.\restorecolumns
    
    -- isReallyBig = xvcmpgtsp vPositive (unfloats $ map (abs . asinh) $ floats @Interp $ maxFloat)
    resultOrMax = posResult -- TODO figure out if it handles big numbers properly maxFloat isReallyBig
    
    
    v2 = xvmulsp vPositive vPositive 
    bitswitch = xvmaddmsp v2 (unwrds4 0x0d000000) (unwrds4 0x0ff00000)
    -- The first nibble is replicated
    zeroOneNibble = vperm bitswitch bitswitch
                        $ unbytes $ [0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12]
    -- and merged with the identity byte permutation to produce a \texttt{vperm}
    -- map which selects between corresponding words in the first and second 
    -- register arguments according to whether the first or second approximation
    -- is used.
    -- This is the key step to balancing the odd and even pipelines,
    -- since this shuffle map is applied five times 
    -- (and used once in the construction of the coefficient map).
    -- It \edchange{WK}{is}{would even be} possible
    -- to construct all of these keys with odd pipeline instructions,
    -- rather than even ones (|xxsel|), 
    -- but this would introduce additional latency which would be harder to schedule,
    -- so we have chosen not to implement this.
    sinhOrExp = xxsel (unbytes [0..15]) zeroOneNibble (unbytes16 16)
    -- This map is first needed to select between the two segment bits for the $\sinh$ 
    -- or $\exp$ lookups.
    segBits = vperm bitswitch expSegBits sinhOrExp
    -- To construct the coefficient lookup, 
    -- we duplicate the segment bits into each byte of the respective word
    segBytes = vperm segBits segBits
                   $ unbytes $ [1,1,1,1, 5,5,5,5, 9,9,9,9, 13,13,13,13]
    -- and insert the segment bits $10$ and $11$ into the in-order lookup key.
    lookupKey = xxsel sinhOrExp segBytes (unbytes16 0x0c)
    -- Calculate $x/log(2)$ for use in $e^x = 2^{x/log(2)}$:

    -- vBylog2 = fma vPositive (unfloats4 $ 1/log(2)*(1+0*1*2**(-24))) (unfloats4 $ -2)
    vBylog2 = xvmaddmsp vPositive (unfloats4 $ 1/log(2)) (unfloats4 $ -2)
    vByHalfLog2 = xvmaddmsp vPositive (unfloats4 $ 1/log(2)) (unfloats4 $ -1)
    
    -- Convert the floating point number to an integer so it can be used as the exponent 
    -- part of the floating point number.  
    -- Multiply by $2^23$ as part of the conversion so the integer part 
    -- is put in the exponent bits ($1$-$8$), 
    -- and the fractional part is put in the mantissa.
    vBylog2AsInt = vctsxs vBylog2 23
    -- Mask out the fractional bits
    -- and add $127\times 2^23$ to get a correctly-biased exponent.
    expntBits = vandc vBylog2AsInt (unwrds4 0x007fffff)
    expPart = vadduwm expntBits (unwrds4 0x3f800000) --Add the bias 
    -- Merge the lower $21$ fractional bits from the integer representation of $|v|/\log 2$
    -- with the exponent for $1$.
    -- The result is a number in the interval $[1,1+1/4)$.
    frac = xxsel (unfloats4 1) vBylog2AsInt (unwrds4 0x001fffff)
    -- The remaining two bits are used to construct a lookup key.
    -- This allows us to evaluate $2^x$ on the four subintervals 
    -- $\cup \{[1,1+1/4),[1+1/4,1+1/2),[1+1/2,1+3/4),[1+3/4,2)\} = [1,2)$.
    expSegBits = vrlq (vrlw vBylog2AsInt (unbytes16 (fromIntegral ((-3) :: Integer)))) (unbytes [0,0,0,0,0,0,0,15])

    -- Use utility function to look up list of coefficients using \texttt{vperm} instructions.
    -- The first coefficient has to be treated separately,
    -- because there are really two types of polynomials being evaluated.
    (c0 : coeffs) = case lookup8Words' lookupKey (mk8WordTbl 2 sinhAndExpC) of
                 [] -> error "Empty list returned from lookup8Words'."
                 xs -> xs


    -- For the fixed segments in $[0,2)$, \eqref{eq:sinhP0}-\eqref{eq:sinhP3},
    -- the inner part of the polynomial is evaluated at $x^2$,
    -- while the outer linear factor is evaluated at $x$.
    -- For the four segments used to evaluate the remainder for the exponential 
    -- computation,  \eqref{eq:sinhP4}-\eqref{eq:sinhP7},
    -- both parts are evaluated on the fractional part in $[1,2)$.
    -- We can use permutation maps to select between the two,
    v2OrFrac = vperm v2 frac sinhOrExp
    vOrFrac = vperm vPositive frac sinhOrExp
    -- and evaluate the appropriate types of polynomials in parallel
    -- for each of the four inputs.
    evalPoly = xvmaddmsp (hornerV coeffs v2OrFrac) vOrFrac roundingOrC0
    -- Since the constant coefficient is zero for the first four segments,
    -- we can substitute a rounding term.  
    -- The simplest rounding term, which is exactly correct near zero,
    -- is to add half an ulp of $x$.
    roundingOrC0 = vperm (xvmulsp vPositive $ unfloats4 $ 1*2**(-24)) c0 sinhOrExp
    -- For the outer intervals, to save an operation, we calculate
    -- \begin{equation}
    -- \sinh x = \left( \frac{e^x}{2} \right) - \frac14 \left( \frac{e^x}{2} \right)^{ -1}.
    -- \end{equation}\restorecolumns
    (halfExpV,_) = expFamilyDev (Flags { lowRange=False, highRange=False, base=Base2, subnormalOutput=False, outputNaN=False, offset=Offset0 }) expCoeffsWithRounding
                 vByHalfLog2
        -- xvmulsp expPart evalPoly
    --halfExpV = xvmulsp expVAbs (unfloats4 0.5)
    invExp2v = recipSP halfExpV
    sinhFromExp = xvnmsubmsp (unfloats4 $ 1/4*(1+2**(-23))) invExp2v halfExpV
    -- where, to get better rounding,
    -- we add an estimated ulp during the multiply-add
    -- which combines the exponentiations of the integral and fractional parts of $x$.
    oneUlp = xvmulsp expPart (unfloats4 $ 2**(-21))
    -- Finally, we select either the straight polynomial evaluation
    -- or the substitution of the polynomial into the
    posResult = vperm evalPoly sinhFromExp sinhOrExp

    isNegInfinity = vcmpgefp (xxland v (unfloats4 (0x7fffffff))) (unfloats4 (0x7fffffff))
    isPosInfinity = xvcmpgtsp v (unfloats4 (0x7f7fffff))  -- 0x7f7fffff = biggest 32-bit floating point number
    isANumber = vcmpeqfp v v

    resultNegInfinity =  xxsel signedResult (unfloats4 $ -1/0) isNegInfinity
    resultInfinity =  xxsel resultNegInfinity (unfloats4 $ 1/0) isPosInfinity
    resultNaN = xxsel (unfloats4 $ 0/0) resultInfinity isANumber

    final = resultNaN

    
  in
    ( final
    , [ 
        ("v",v)
      , ("vPositive", vPositive)
      , ("signedResult ", signedResult )
      , ("resultOrMax ", resultOrMax )
      , ("v2 ", v2 )
      , ("bitswitch ", bitswitch )
      , ("zeroOneNibble ", zeroOneNibble )
      , ("sinhOrExp ", sinhOrExp )
      , ("segBits ", segBits )
      , ("segBytes ", segBytes )
      , ("lookupKey ", lookupKey )
      , ("vBylog2 ", vBylog2 )
      , ("vByHalfLog2", vByHalfLog2)
      , ("vBylog2AsInt ", vBylog2AsInt )
      , ("exponentBits ", expntBits )
      , ("expPart ", expPart )
      , ("frac ", frac )
      , ("expSegBits ", expSegBits )
      , ("v2OrFrac ", v2OrFrac )
      , ("vOrFrac ", vOrFrac )
      , ("evalPoly ", evalPoly )
      , ("roundingOrC0 ", roundingOrC0 )
      , ("halfExpV ", halfExpV )
      , ("invExp2v ", invExp2v )
      , ("sinhFromExp ", sinhFromExp )
      , ("oneUlp ", oneUlp )
      , ("posResult ", posResult )
      , ("final",final)
      ]
    )
{-
A single fused multiply-add with suitably chosen constants generates two switches:
Bit $3$ switches between the two approximations, 
and bits $10$ and $11$ select the interval:
\begin{center}
\begin{tabular}{cc}
\hline
v              & \texttt{bitSwitch}\cr
\hline
$0$              & \texttt{0x0ff00000} \cr
$1$              & \texttt{0x0ff40000} \cr
$\sqrt{2}      $ & \texttt{0x0ff7ffff} \cr
$\sqrt{2}+\ulp $ & \texttt{0x0ff80000} \cr
$\sqrt{3}      $ & \texttt{0x0ffbffff} \cr
$\sqrt{3}+\ulp $ & \texttt{0x0ffc0000} \cr
$2       -\ulp $ & \texttt{0x0fffffff} \cr
$2             $ & \texttt{0x10000000} \cr
$2^{19}-2^{ -5}   $ & \texttt{0x1ffffffe} \cr
$2^{19}          $ & \texttt{0x20000000} \cr
\hline
\end{tabular}
\end{center}
-}

-- The polynomial coefficients, computed by Maple, corresponding to \figref{sinh} are
sinhAndExpC :: Floating b => [[b]]
sinhAndExpC = map (map (*(1+2**(-24)))) 
  [ [0, 0, 0, 0
    , 1.004562488039534617244713, 1.194632858241450449641738
    , 1.420665894836769938361546, 1.689465990181594301004609]
  , [1.000000000000000000000000, 0.9999860536222566970485636
  , 0.9998844038377509884253232, 0.9995352508436275796090500
    , 0.6732278748825172121904292, 0.8006073788284511559050211
    , 0.9520879912264729850610229, 1.132229813275169945121840]
  , [0.1666671824671209211405940, 0.1667053815835268341666265
  , 0.1668534166396276046209211, 0.1671978080633088776623829
    , 0.2745502974588843921140253, 0.3264971671642188086141066
    , 0.3882727542199218012821016, 0.4617367218800337969138400]
  , [0.8330211426011138249662972e-2, 0.8293940074044863416903818e-2
  , 0.8221394923904505452632296e-2, 0.8107660583408454955870616e-2
    , 0.2667298693692388827296378e-1, 0.3171970584376452320035350e-1
    , 0.3772129987519816075815080e-1, 0.4485843819873690697265410e-1]
  , [0.2037740427530126721867954e-3, 0.2157847654819105108513454e-3
  , 0.2277531706576630700655594e-3, 0.2403301492636051772991948e-3
    , 0.2098647744015674352851058e-1, 0.2495726829067849170775247e-1
    , 0.2967936102230666092019852e-1, 0.3529490729646151440167639e-1]
  ]

