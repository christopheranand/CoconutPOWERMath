-- |
-- Module      :  Coconut.Core.Interp
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for interpreting the @PowerISA@ DSL

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, ExistentialQuantification, TypeFamilies, TypeApplications, ScopedTypeVariables, InstanceSigs, DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes, TupleSections #-}


module ISA.PowerInterp (module ISA.PowerInterp,module Coconut.Core.Interp) where

import qualified Data.List as List
import qualified Data.ByteString as BS

import Coconut.BaseTypes
import Coconut.Core.CoreISA
import Coconut.Core.Interp

import ISA.PowerISA
import Coconut.Utils.RunTimeSized
import Coconut.Utils.ArbFloat (dbl2Word32, dbl2Word64, chop, word322Dbl, word642Dbl, FPClass(..), af2DVal, dval2af)
import Coconut.Utils.ArbFloat

import Data.Int (Int64, Int8, Int16, Int32)
import Data.Word (Word64, Word32, Word16, Word8)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Bits as Bits (xor, rotateL, complement, shift, shiftR, shiftL, (.&.), rotate, (.|.), countLeadingZeros)
import Data.Proxy (Proxy(..))
import Data.Word as Unsigned

import GHC.Arr (inRange)
import GHC.TypeLits

import Numeric (showHex)

-- || HELPER FUNCTIONS FROM OLD PowerInterp.lhs|| --

-- unsign :: (Integral b, Num a, Ord a, Show b) => b -> a -> a
unsign b v  | b < 1     = error $ "PPCAC.unsign bit "++show b
            | v < 0     = 2^b + v
            | otherwise = v

cmpHelper from to cmp ra rb = to $ zipWith (\ a -> \ b -> if cmp a b then -1 else 0) (from ra) (from rb)

-- || HELPER FUNCTIONS || --

-- [ if n >= x && n <= y then 1 else 0 | n <- [0..127]]
mask128 x y =
  if x <= y then 2^(128 - x) - 2^(127 - y)
  else 2^(128) - 2^(127 - y) + 2^(128 - x) - 1

mask32 x y =
  if x <= y then 2^(32 - x) - 2^(31 - y)
  else 2^(32) - 2^(31 - y) + 2^(32 - x) - 1

encodeWord64 :: Word64 -> [Word32]
encodeWord64 x = map fromIntegral [ x .&. 0xFFFFFFFF, (x .&. 0xFFFFFFFF00000000) `shiftR` 32 ]

-- | Merge helper functions
-- It would be great if somebody could test the mergehigh and mergelow implementations to double check my code!
merge [] [] = []
merge (a1:as) (b1:bs) = a1:b1:merge as bs
merge a b = error $ "ZInterp.mergeHi illegal "++show(a,b)

first_halve :: [a] -> [a]
first_halve = (\xs -> case xs of
            [] -> []
            xs -> take ((length xs) `div` 2 ) xs)

second_halve :: [a] -> [a]
second_halve = (\xs -> case xs of
            [] -> []
            xs -> drop ((length xs) `div` 2 ) xs)

mergeEven [] [] = []
mergeEven (a1:_:as) (b1:_:bs) = a1:b1:mergeEven as bs
mergeEven a b = error $ "ZInterp.mergeEven illegal "++show(a,b)

mergeOdd [] [] = []
mergeOdd (_:a1:as) (_:b1:bs) = a1:b1:mergeOdd as bs
mergeOdd a b = error $ "ZInterp.mergeOdd illegal "++show(a,b)

floatOp2 :: (Double -> Double -> Double) -> Interp VR -> Interp VR -> Interp VR
floatOp2 f x y = unfloats $ zipWith f (floats x) (floats y)

floatOp3 :: (Double -> Double -> Double -> Double) -> Interp VR -> Interp VR -> Interp VR -> Interp VR
floatOp3 f x y z = unfloats $ zipWith3 f (floats x) (floats y) (floats z)

instance PowerISA Interp where

-- || Load / Stores || --
  lvx mr d = case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let idx = fromIntegral $ (signedG d)
          idx' = (idx,idx+15)
      in if all (inRange (0,BS.length vals-1)) [idx,idx+15]
        then (undwrds $ map bytes2Dwrd $ chunks 8 $ BS.unpack $ BS.take 16 $ BS.drop idx vals,
              InterpMR $ PIMR name vals atMeet (Load idx':ops) size)
        else error $ "PowerInterp.lvx index out of range. d: " ++ show d
                   ++ " length vals: " ++ show (BS.length vals)
  stvx mr d v = case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let idx = (fromIntegral $ runInterpGPR d)
          idx' = (idx, idx+15)
          vrbytes = BS.pack $ uncurry (<>) $ pupdD (chop 8) $ runInterpVR v
      in if all (inRange (0,BS.length vals-1)) [idx,idx+15]
         then InterpMR $ PIMR name (spliceByteString vrbytes vals idx') atMeet (Store idx' vrbytes:ops) size
         else error $ "PowerInterp.stvx index out of range " ++ take 300 (show (idx,name,vals))


  lxv mr d = case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let idx = d
          idx' = (idx,idx+15)
      in if all (inRange (0,BS.length vals-1)) [idx,idx+15]
        then (undwrds $ map bytes2Dwrd $ chunks 8 $ BS.unpack $ BS.take 16 $ BS.drop idx vals,
              InterpMR $ PIMR name vals atMeet (Load idx':ops) size)
        else error $ "PowerInterp.lvx index out of range. d: " ++ show d
                   ++ " length vals: " ++ show (BS.length vals)
  stxv mr d v = case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let idx = d
          idx' = (idx, idx+15)
          vrbytes = BS.pack $ uncurry (<>) $ pupdD (chop 8) $ runInterpVR v
      in if all (inRange (0,BS.length vals-1)) [idx,idx+15]
         then InterpMR $ PIMR name (spliceByteString vrbytes vals idx') atMeet (Store idx' vrbytes:ops) size
         else error $ "PowerInterp.stvx index out of range " ++ take 300 (show (idx,name,vals))

-- || FUNCTIONS TO DO ARITHMETIC || --
  absG ra = unintegerSG $ fromIntegral $ Prelude.abs(signedG ra)

  addcG ra rb = InterpGPR $ runInterpGPR ra + runInterpGPR rb
  addiG ra si = InterpGPR $ runInterpGPR ra + fromIntegral si
  subfG ra rb = InterpGPR $ runInterpGPR ra - runInterpGPR rb
  subficG ra ib = InterpGPR $ runInterpGPR ra - fromIntegral ib
  negG ra = InterpGPR $ negate $ runInterpGPR ra
  mulldG ra rb  = InterpGPR $ runInterpGPR ra * runInterpGPR rb
  mulhdG ra rb  = InterpGPR . fromIntegral . flip div (2^64) $ (signedG ra * signedG rb)
  mulhduG ra rb = InterpGPR . fromIntegral . flip div (2^64) $ (unsignedG ra * unsignedG rb)
  divdG ra rb   = InterpGPR . fromIntegral $ (signedG ra `div` signedG rb)
  divduG ra rb  = InterpGPR . fromIntegral $ (unsignedG ra `div` unsignedG rb)

  rldiclG rs sh mb = InterpGPR $ Bits.rotateL (runInterpGPR rs) (fromIntegral sh)  .&. (2^(63-mb+1) - 1)
  rldicrG rs sh me = InterpGPR $ Bits.rotateL (runInterpGPR rs) (fromIntegral sh)  .&. (2^(63+1) - 2^(63-me))
  rldicG rs sh mb = InterpGPR $ Bits.rotateL (runInterpGPR rs) (fromIntegral sh)  .&. (2^(63+mb+1) - 2^(63-sh))
  rldimiG ra rb sh mb = let mask = (2^(64-mb) - 2^sh)
      in InterpGPR $ Bits.rotateL (runInterpGPR rb) (fromIntegral sh) .&. mask
                  .|. runInterpGPR ra .&. complement mask
  rldclG ra rb = error "ZInterp.rldcl"
  rldcrG ra rb = error "ZInterp.rldcr"
  rlwimiG ra rb sh mb me = let mask = (2^(32-mb) - 2^(31-me)) in InterpGPR $
    fromIntegral $ rotateLeft32 (fromIntegral $ runInterpGPR rb) (fromIntegral sh) .&. mask
               .|. fromIntegral (runInterpGPR ra) .&. complement mask

  cmp ra rb bf = case runInterpGPR bf of 0 -> if runInterpGPR ra < runInterpGPR rb then InterpGPR $ 1 else InterpGPR $ 0
                                         1 -> if runInterpGPR ra > runInterpGPR rb then InterpGPR $ 1 else InterpGPR $ 0
                                         2 -> if runInterpGPR ra == runInterpGPR rb then InterpGPR $ 1 else InterpGPR $ 0

  add ra rb = InterpGPR $ runInterpGPR ra + runInterpGPR rb

  xoriG ra ui = InterpGPR $ fromIntegral $ signedG ra `xor` fromIntegral ui

  eqvG ra rb =  InterpGPR $ complement $ fromIntegral $ signedG ra `xor` signedG rb

  nandG ra rb = InterpGPR $ complement $ fromIntegral $ signedG ra .&. signedG rb

  -- orG ra rb = InterpGPR $ fromIntegral $ signedG ra .|. signedG rb

  norG ra rb = InterpGPR $ complement $ fromIntegral $ signedG ra .|. signedG rb

  -- xorisG ra ui =
  --   let
  --     upperBits = signedG ra .&. complement 0xFFFF
  --     xorResult = upperBits .|. (fromIntegral ui .&. 0xFFFF)
  --   in InterpGPR $ fromIntegral xorResult


  divdu ra rb  = InterpGPR . fromIntegral $ (unsignedG ra `div` unsignedG rb)

  andc ra rb = InterpGPR $ runInterpGPR ra .&. complement (runInterpGPR rb)

  creqv ba bb = InterpCR $ complement (runInterpCR ba `Bits.xor` runInterpCR bb)

  crand ba bb = InterpCR $ runInterpCR ba .&. runInterpCR bb

  crandc ba bb = InterpCR $ runInterpCR ba .&. complement (runInterpCR bb)

  crnand ba bb = InterpCR $ complement (runInterpCR ba .&. runInterpCR bb)

  crnor ba bb = InterpCR $ complement (runInterpCR ba .|. runInterpCR bb)

  crxor ba bb = InterpCR $ runInterpCR ba `Bits.xor` runInterpCR bb

  crorc ba bb = InterpCR $ runInterpCR ba .|. complement (runInterpCR bb)

  eqv rs rb = InterpGPR $ runInterpGPR rs `Bits.xor` runInterpGPR rb

-- || FUNCTIONS TO COUNT LEADING ZEROS || --
  cntlzd rs = unintegerSG $ fromIntegral $ countLeadingZeros $ signedG rs
  cntlzw rs = addcG (unintegerSG ( fromIntegral ( countLeadingZeros (signedG rs .&. 0x00000000FFFFFFFF)))) (unintegerSG (-32))



-- || VECTOR INSTRUCTIONS || --

{- Vector Select Intructions -}
  vsel ra rb rc = undwrds $ zipWith3 (\ a b sel -> sel .&. b .|. complement sel .&. a)
                                     (dwrds ra) (dwrds rb) (dwrds rc)

  -- TODO vsel interp implemnted the same as vsel (without edge cases)
  xxsel ra rb rc = undwrds $ zipWith3 (\ a b sel -> sel .&. b .|. complement sel .&. a)
                                     (dwrds ra) (dwrds rb) (dwrds rc)
{- Vector Permute Intructions -}
  vperm ra rb rc = unbytes $ map vperm' $ bytes rc
    where
      all = bytes ra ++ bytes rb
      vperm' x = all !! fromIntegral (mod x 32)

  vpermr ra rb rc = unbytes $ map vperm' $ bytes rc
    where
      all = bytes ra ++ bytes rb
      vperm' x = all !! (31 - fromIntegral (mod x 32))

{- Vector Permute Intructions -}
  vclzb ra = unbytes $ map (fromIntegral . length . fst . span (== 0)) $ chunksOf 8 $ bits ra
  vclzh ra = unshorts $ map (fromIntegral . length . fst . span (== 0)) $ chunksOf 16 $ bits ra
  vclzw ra = unwrds $ map (fromIntegral . length . fst . span (== 0)) $ chunksOf 32 $ bits ra
  vclzd ra = undwrds $ map (fromIntegral . length . fst . span (== 0)) $ chunksOf 64 $ bits ra
  vclzdm ra rb = vclzd (vand ra rb )

{- Vector Splat Instructions -}
  vspltb va uim = unbytes $ replicate 16 ( (bytes va) !! (fromIntegral $ mod uim 16) )

  {- Vector Merge Instructions -}

  vmrghb va vb = unbytes $ merge (first_halve(bytes va)) (first_halve(bytes vb))
  vmrglb va vb = unbytes $ merge (second_halve(bytes va)) (second_halve(bytes vb))

  vmrghh va vb = unshorts $ merge (first_halve(shorts va)) (first_halve(shorts vb))
  vmrglh va vb = unshorts $ merge (second_halve(shorts va)) (second_halve(shorts vb))

  vmrghw va vb = unwrds $ merge (first_halve(wrds va)) (first_halve(wrds vb))
  vmrglw va vb = unwrds $ merge (second_halve(wrds va)) (second_halve(wrds vb))

  vmrgew va vb = unwrds $ mergeEven (wrds va) (wrds vb)
  vmrgow va vb = unwrds $ mergeOdd (wrds va) (wrds vb)

  {- Vector absolute value instructions -}
  -- xvabssp va =
  --   let
  --     a = wrds va :: [Unsigned.Word32]
  --   in
  --     if
  --     unwrds' $ zipWith (Prelude.abs) (wrds va :: [Unsigned.Word32]) --(\ a -> Prelude.abs(a))

  -- vadduwm va vb = unwrds' $ zipWith (+) (wrds va :: [Unsigned.Word32]) (wrds vb)

  {- Vector Floating Point Arithmetic Instructions -}
  xvmaddmdp   = doubleOp3 (\ a b c -> a*b+c)
  xvmsubmdp   = doubleOp3 (\ a b c -> a*b-c)
  xvnmaddmdp   = doubleOp3 (\ a b c -> -(a*b+c))
  xvnmsubmdp   = doubleOp3 (\ a b c -> -a*b+c)
  vor     = dwrdOp2 (.|.)
  xxlor   = dwrdOp2 (.|.)
  vorc    = dwrdOp2 (\ x y -> x .|. Bits.complement y)
  vxor    = dwrdOp2 Bits.xor
  vxorc   = dwrdOp2 (\ x y -> x `xor` Bits.complement y)
  vand    = dwrdOp2 (.&.)
  xxland  = dwrdOp2 (.&.)
  vandc   = dwrdOp2 (\ x y -> x .&. Bits.complement y)
  xvadddp    = doubleOp2 (+)
  xvsubdp    = doubleOp2 (-)
  xvmuldp    = doubleOp2 (*)

  xvaddsp    = floatOp2 (+)
  xvsubsp    = floatOp2 (-)
  xvmulsp    = floatOp2 (*)
  xvmaddmsp   = floatOp3 (\ a b c -> a*b+c)
  xvnmaddmsp  = floatOp3 (\ a b c -> -(a*b+c))
  xvmsubmsp   = floatOp3 (\ a b c -> a*b-c)
  xvnmsubmsp  = floatOp3 (\ a b c -> -a*b+c)



{- Functions from Old PowerInterp.lhs-}

{- Functions to replace old instructions -}
  -- | Vector Converts | --
  -- Vector Convert with round Double-Precision to Single-Precision
  xvcvdpsp xb = unfloats $ concatMap (\x -> [x,x]) $ doubles xb

  -- | Vector Shifts | --
  -- vector shift left
  vslw va vb =
    let src1 = wrds va
        src2 = (wrds vb)
    in
      unwrds $ zipWith (\a b -> Bits.shift a (fromIntegral (b .&. 0x1F))) src1 src2

-- vector shift right
  vsrw va vb =
    let src1 = wrds va
        src2 = (wrds vb)
    in
      unwrds $ zipWith (\a b -> Bits.shift a (- fromIntegral (b .&. 0x1F))) src1 src2

  -- vector shift right algebraic (sign extended)
  vsraw va vb =
    let src1 = wrds va
        src2 = (wrds vb)
    in
      unwrds $ zipWith (\a b -> let
                                  sh = (fromIntegral (b .&. 0x1F))
                                in
                                  if a >= 2^31 then (Bits.shift a (-sh)) + (fromIntegral $ (2::Integer)^(32) -2^(32-(fromIntegral sh) )) -- add in 1s for bits shifted in (since this is unsigned)
                                             else Bits.shift a (-sh)) src1 src2

  -- | Vector Rotates | --
  vrlwmi vt va vb =
    let
      rlwmi aa bb tt =
        let
          list = bytes vb
          b = (list !! 1) .&. 0x1F
          e = (list !! 2) .&. 0x1F
          n = bb .&. 0x1F
          r = Bits.rotateL (aa) (fromIntegral n)
          m = mask32 b e
        in
          (r .&. m) .|. (fromIntegral tt) .&. (Bits.complement m)
    in unwrds $ map fromIntegral $ zipWith3 rlwmi (wrds va) (wrds vb) (wrds vt) -- swapped b and e to get  complement of mask

  vrlqmi vt va vb =
    let
      list = bytes vb
      b = (list !! 5) .&. 0x7F
      e = (list !! 6) .&. 0x7F
      n = (list !! 7) .&. 0x7F
      r = Bits.rotateL (integer va) (fromIntegral n)
      m = mask128 b e
    in uninteger $ (r .&. m) .|. ((integer vt) .&. (mask128 e b)) -- swapped b and e to get  complement of mask

  -- replace roti (double check implementation)
  vrlw va vb = unwrds $ zipWith (\a b -> Bits.rotateL a (b .&. 0x1F)) (wrds va) (map fromIntegral $ wrds vb)

  vrlq va vb = --replace rotqbii
    let
      list = bytes vb
      n = (list !! 7) .&. 0x7F
      (quot,rem) = divMod (integer va) (2^(128 - n))
      va' = Bits.shiftL rem (fromIntegral n) + quot
    in uninteger va'


  -- | Vector Compares | -

  -- Compare a vector and an unsigned byte (vb) -- replaces cgtbi
  vcmpgtub va vb = unbytes $ zipWith (\a b -> if a > b then 0xFF else 0) (bytes va) (bytes vb)

  -- compare two vectors >=
  vcmpgefp va vb = unwrds $ zipWith (\a b -> if a >= b then 0xFFFFFFFF else 0) (floats va) (floats vb)

  -- compare two vectors >
  vcmpgtfp va vb = unwrds $ zipWith (\a b -> if a > b then 0xFFFFFFFF else 0) (floats va) (floats vb)
  -- TODO xvcmpgtsp implemented same as vcmpgtfp (missing edge cases)
  xvcmpgtsp va vb = unwrds $ zipWith (\a b -> if a > b then 0xFFFFFFFF else 0) (floats va) (floats vb)

  -- Vector Compare Equal Unsigned Word VC-form
  vcmpequw va vb = unwrds $ zipWith (\a b -> if a == b then 0xFFFFFFFF else 0) (wrds va) (wrds vb)

   -- Compare two vectors =
  vcmpeqfp va vb = unwrds $ zipWith (\a b -> if a == b then 0xFFFFFFFF else 0) (floats va) (floats vb)

  vcmpbfp va vb = unwrds $ zipWith (\a b ->
                                      if (abs b) > a && a > b then 0
                                      else if a > b && a < (-b) then 0xc0000000
                                      else if a > b then 0x80000000
                                      else 0x20000000
                                    )
                                    (floats va) (floats vb)


  -- | Convert floats to integers and integers to floats | --

  vcfsx src uim = unfloats $ (map (* ((0.5::Double)^^uim))) $ map (fromIntegral) $ int32s src --replaces csflt
  vcfux src uim = unfloats $ (map (* ((0.5::Double)^^uim))) $ map (fromIntegral) $ wrds src --replaces cuflt

  vctsxs src uim = unint32s $ map (truncate) $ (map (* ((2::Double)^^uim))) $ floats src --replaces cflts
  vctuxs src uim = unwrds $ map (truncate) $ (map (* ((2::Double)^^uim))) $ floats src -- replaces cfltu

  -- Elementwise Word Addition
  -- vaddsws v1 v2 = error "unwrds' $ zipWith (+) (wrds v1 :: [Unsigned.Word32]) (wrds v2)" --doesn't saturate
  -- Vector Add Unsigned Word Modulo
  vadduwm va vb = unwrds' $ zipWith (+) (wrds va :: [Unsigned.Word32]) (wrds vb)

  -- mpya v1 v2 v3 =
  --   let
  --     intermediate = vmulosh v1 v2
  --   in
  --     vaddsws intermediate v3
  -- mpyui ra imm = vmulosh ra (unwrds4 imm)
  -- mpyi ra imm = vmulosh ra (unint32s imm)
  -- mpy ra rb = vmulosh ra rb



  -- vmulhsw v1 v2 =             -- TODO: Add correct implementation?
  --   let
  --     src1 = wrds v1
  --     src2 = wrds v2
  --   in
  --     unwrds' $ zipWith (*) (src1) (src2)

  vmulosh v1 v2 =
    let
      mulSignExtend :: Word32 -> Word32 -> Integer
      mulSignExtend a b = (sExt 16 16 $ fromIntegral $ a .&. 0xFFFF) * (sExt 16 16 $ fromIntegral $ b .&. 0xFFFF)
    in
      unwrds $ map fromIntegral $ zipWith (mulSignExtend) (wrds v1) (wrds v2)

  vmuleuh v1 v2 =
    let
      mul a b = (Bits.shiftR a 16) * (Bits.shiftR b 16)
    in
      unwrds $ map fromIntegral $ zipWith (mul) (wrds v1) (wrds v2)



  -- mpya v1 v2 v3 =
  --   let
  --     intermediate = vmulosh v1 v2
  --   in
  --     vaddsws intermediate v3
  -- mpyui ra imm = vmulosh ra (unwrds4 imm)
  -- mpyi ra imm = vmulosh ra (unint32s imm)
  -- mpy ra rb = vmulosh ra rb
  vaddsws v1 v2 = error "unwrds' $ zipWith (+) (wrds v1 :: [Unsigned.Word32]) (wrds v2)" --doesn't saturate
  -- Vector Add Unsigned Word Modulo
  vsubuwm va vb = unwrds' $ zipWith (-) (wrds va :: [Unsigned.Word32]) (wrds vb)

  -- Vector Modulo
  vmodsw va vb = unint32s $ zipWith (Prelude.mod) (int32s va) (int32s vb)

  -- | Vector Negate | --
  vnegw vb = unwrds $ map (Prelude.negate) (wrds vb)
  -- vnegw vb = unwrds $ map (\ x -> 1 + (Bits.complement x)) (wrds vb)

  -- | Sign Extends | --
  s10      = sExt 10  32

  -- | Vector Reciprocals | --
  -- not sure, sure?
  -- vrefp v = unfloats $ map ({-estimate?-}recip) (floats v)
  vrefp v = (vand (unwrds4 0xfffffc00)
                      $ unfloats $ map (\x-> recip x) (floats v))

  vrsqrtefp v = (vand (unwrds4 0xfffffc00)
        $ unfloats $ map (\x-> recip $ sqrt x) (floats v))

  vinsw v g i =
    let
      element = unwrds @Interp [(fromIntegral $ unsignedG $andG g (unintegerG 0x00000000FFFFFFFF)) :: Word32]
      elementl = take 4 $ bytes element
      l :: [Word8]
      l = bytes v
    in
      if i <= 12 then
      unbytes((take i l) ++ elementl ++ (drop (i+4) l))
      else error "max immediate value is 12"

-- | Extra Unused Code | --

tupleWord64ToListInt :: (Word64, Word64) -> [Int]
tupleWord64ToListInt (h, l) = map fromIntegral [h, l]
