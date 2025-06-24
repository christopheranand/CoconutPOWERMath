{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module MathUtils(listMatch,qMapPPP,qMapPoP,qMapooP,qMapoPP,qMapPoo,qMapPPo,qMapoPo,qMap,qMapPP,qMapPo,qMapoP,eightMap,qTranspose,qFold1,qFoldZip,qSplat,qSplat123,merge3High,subDbg,assert,MOBase(..),MOExceptional(..),moAll,MathOptions(..),pMapoPoP,pMapPPPP,pMapPPoP,pMapPPP,pMapPoP,pMapooP,pMapoPP,pMapPoo,pMapPPo,pMapoPo,pMap,pMapPP,pMapPo,pMapoP,pFoldoP,wrd2P,copySignFlt,signBitFlt,copySignDbl,signBitDbl,nanDbl,infDbl,maxDbl,zero,bitsMatch,lookupWordsPair4,mergeSplit3Quads,mergeDbls,dblExpBits,dblExpBitsInWord,mkDblUpDownKeyBool,mkDblUpDownKey,mkDblSignUpDownKey,mkDblSignLRKey,hornerGen,hornerV,hornerVDbl,hornerPDbl,Polynomial(..),polyHorner,polyFudge,polyFudge',horner4,horner2Dbl,horner2w64,horner,hornerDbl,onePlusMant,extractExp,mapWord,mapword,key8Word,mk8WordTbl,mk8WordPair,lookup8Word,lookup8WordDev,lookup8Word',lookup8Words',bitWidthErr,testCoeffs,mk16WordTbl,mk16WordPair,in4s,lookup16X2,l16X2l,RegLookupSpec(..),lookupBits,lookupBreaks,evenSPBreaks,roundToSP,lookupMemo,LookupSpec(..),calcBreaks,lookup16X2Coeffs,lookupQuadsX2Coeffs,use16X2lookup,use16X2lookupDev,splat,shufB,shufB1,cmpsi,cgtbi) where

import ISA.PowerISA
import ISA.PowerInterp as PowerInterp

import PrelExts

import Coconut.BaseTypes

import Data.Word (Word64, Word32, Word8)
import qualified Data.List as List

import Data.Bits as Bits

-- import BinaryNumber(NatE,listMatch) 
listMatch :: a
listMatch = error"Import not found"

-- type Quad v = (v,v,v,v)

qMapPPP :: (t1 -> t2 -> t3 -> d) -> (t1, t1, t1, t1) -> (t2, t2, t2, t2) -> (t3, t3, t3, t3) -> (d, d, d, d)
qMapPPP f (x1,x2,x3,x4) (y1,y2,y3,y4) (z1,z2,z3,z4)  = (f x1 y1 z1,  f x2 y2 z2,  f x3 y3 z3,  f x4 y4 z4)
qMapPoP :: (t1 -> t2 -> t3 -> d) -> (t1, t1, t1, t1) -> t2 -> (t3, t3, t3, t3) -> (d, d, d, d)
qMapPoP f (x1,x2,x3,x4) y (z1,z2,z3,z4)              = (f x1 y z1,   f x2 y z2,   f x3 y z3,   f x4 y z4)
qMapooP :: (t1 -> t2 -> t3 -> d) -> t1 -> t2 -> (t3, t3, t3, t3) -> (d, d, d, d)
qMapooP f x y (z1,z2,z3,z4)                          = (f x y z1,    f x y z2,    f x y z3,    f x y z4)
qMapoPP :: (t1 -> t2 -> t3 -> d) -> t1 -> (t2, t2, t2, t2) -> (t3, t3, t3, t3) -> (d, d, d, d)
qMapoPP f x (y1,y2,y3,y4) (z1,z2,z3,z4)              = (f x y1 z1,   f x y2 z2,   f x y3 z3,   f x y4 z4)
qMapPoo :: (t1 -> t2 -> t3 -> d) -> (t1, t1, t1, t1) -> t2 -> t3 -> (d, d, d, d)
qMapPoo f (x1,x2,x3,x4) y z                          = (f x1 y z,    f x2 y z,    f x3 y z,    f x4 y z)
qMapPPo :: (t1 -> t2 -> t3 -> d) -> (t1, t1, t1, t1) -> (t2, t2, t2, t2) -> t3 -> (d, d, d, d)
qMapPPo f (x1,x2,x3,x4) (y1,y2,y3,y4) z              = (f x1 y1 z,   f x2 y2 z,   f x3 y3 z,   f x4 y4 z)
qMapoPo :: (t1 -> t2 -> t3 -> d) -> t1 -> (t2, t2, t2, t2) -> t3 -> (d, d, d, d)
qMapoPo f x (y1,y2,y3,y4) z                          = (f x y1 z,    f x y2 z,    f x y3 z,    f x y4 z)

qMap :: (t -> d) -> (t, t, t, t) -> (d, d, d, d)
qMap f (x1,x2,x3,x4) = (f x1, f x2, f x3, f x4)

eightMap :: (t -> h) -> (t, t, t, t, t, t, t, t) -> (h, h, h, h, h, h, h, h)
eightMap f (x1,x2,x3,x4,x5,x6,x7,x8) = (f x1, f x2, f x3, f x4
                                       ,f x5, f x6, f x7, f x8)

qMapPP :: (t1 -> t2 -> d) -> (t1, t1, t1, t1) -> (t2, t2, t2, t2) -> (d, d, d, d)
qMapPP f (x1,x2,x3,x4) (y1,y2,y3,y4)  = (f x1 y1, f x2 y2, f x3 y3, f x4 y4)
qMapPo :: (t1 -> t2 -> d) -> (t1, t1, t1, t1) -> t2 -> (d, d, d, d)
qMapPo f (x1,x2,x3,x4) y  = (f x1 y, f x2 y, f x3 y, f x4 y)
qMapoP :: (t1 -> t2 -> d) -> t1 -> (t2, t2, t2, t2) -> (d, d, d, d)
qMapoP f x (y1,y2,y3,y4)  = (f x y1, f x y2, f x y3, f x y4)

-- Transpose 4x4 in registers.
qTranspose :: PowerISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
qTranspose (v1,v2,v3,v4) = (vperm t1 t3 tr3
                      ,vperm t1 t3 tr4
                      ,vperm t2 t4 tr3
                      ,vperm t2 t4 tr4)

  where t1 = vperm v1 v2 tr1
        t2 = vperm v1 v2 tr2
        t3 = vperm v3 v4 tr1
        t4 = vperm v3 v4 tr2

        tr1 = unbytes [ 0, 1, 2, 3, 4, 5, 6, 7,16,17,18,19,20,21,22,23 :: Word8]
        tr2 = unbytes [ 8, 9,10,11,12,13,14,15,24,25,26,27,28,29,30,31 :: Word8]
        tr3 = unbytes [ 0, 1, 2, 3, 8, 9,10,11,16,17,18,19,24,25,26,27 :: Word8]
        tr4 = unbytes [ 4, 5, 6, 7,12,13,14,15,20,21,22,23,28,29,30,31 :: Word8]

-- |pFoldoP f x (y1,y2)  = f x y1 y2|
qFold1 :: (t -> t -> t) -> (t, t, t, t) -> t
qFold1 f (x1,x2,x3,x4) = f (f x1 x2) (f x3 x4)
qFoldZip :: (t1 -> t2 -> t3 -> t3) -> (t1, t1, t1, t1) -> (t2, t2, t2, t2) -> t3 -> t3
qFoldZip f (x1,x2,x3,x4) (y1,y2,y3,y4) accum = f x1 y1 (f x2 y2 (f x3 y3 (f x4 y4 accum)))

qSplat :: PowerISA repr => repr VR -> (repr VR, repr VR, repr VR, repr VR)
qSplat v = (shufB v v  $ map (+0)   [0,1,2,3,  0,1,2,3,  0,1,2,3,  0,1,2,3 :: Word8]
           ,shufB v v  $ map (+4)   [0,1,2,3,  0,1,2,3,  0,1,2,3,  0,1,2,3 :: Word8]
           ,shufB v v  $ map (+8)   [0,1,2,3,  0,1,2,3,  0,1,2,3,  0,1,2,3 :: Word8]
           ,shufB v v  $ map (+12)  [0,1,2,3,  0,1,2,3,  0,1,2,3,  0,1,2,3 :: Word8]
           )
qSplat123 :: PowerISA repr => repr VR -> (repr VR, repr VR, repr VR, repr VR)
qSplat123 v = (shufB v v  $ map (+0)   [0,1,2,3,  0,1,2,3,  0,1,2,3,  0x80, 0x80, 0x80, 0x80 :: Word8]
              ,shufB v v  $ map (+4)   [0,1,2,3,  0,1,2,3,  0,1,2,3,  0x80, 0x80, 0x80, 0x80 :: Word8]
              ,shufB v v  $ map (+8)   [0,1,2,3,  0,1,2,3,  0,1,2,3,  0x80, 0x80, 0x80, 0x80 :: Word8]
              ,shufB v v  $ map (+12)  [0,1,2,3,  0,1,2,3,  0,1,2,3,  0x80, 0x80, 0x80, 0x80 :: Word8]
              )

-- Merge bytes from two different doubles, to save table space.
-- Assume |bytes1 + bytes2 + sndBytes <= 16|, and |length lstA == length lstB|.
merge3High :: (Int,Int) -> Int -> [[Word64]] -> [Word64] -> [[Word64]]
merge3High (bytes1,bytes2) sndBytes lstA lstB = if bytes1 + bytes2 + sndBytes > 16
    then error $ "InverseTrigDbl.merge3High "++show (bytes1,bytes2,sndBytes)
    else zipWith munge lstA lstB
  where
    munge :: [Word64] -> Word64 -> [Word64]
    munge a b = dwrds @Interp $ shufB (undwrds (a::[Word64])) (undwrds2 (b::Word64)) $ map fromIntegral $ [0..bytes1-1] ++ [16..23-bytes1] ++ [8..7+bytes2] ++ [24-bytes1..31-bytes1-bytes2 :: Int]


-- This is for standard debugging, but we need to put it here or |PrelExts|.
-- It adds a prefix to variables included from fu
subDbg :: [a] -> [([a], b)] -> [([a], b)]
subDbg prefix = map $ pupd1 (prefix ++)
-- |subDbg = map . pupd1 . (++)|

-- Make assertions
assert :: Bool -> String -> a -> a
assert test str x = if test then x else error $ "assertion " ++ str ++ " failed"



{- Options for generating functions -}
data MOBase = MO2 | MO10 | MOe | MOeX2 | MOep1 | MOem1 | MO2p1 | MO2m1 | MOpi deriving (Show, Eq)

data MOExceptional = MONaN | MOInf | MOPInf | MOMInf | MOZero | MOPZero | MOMZero | MOSubnormal
                   deriving (Show, Eq)

moAll :: [MOExceptional]
moAll = [MONaN, MOInf, MOPInf, MOMInf, MOZero, MOPZero, MOMZero, MOSubnormal]
data MathOptions = MathOptions  {moBase :: MOBase
                                ,moHandle :: [MOExceptional]
                                }

-- Bit rotate
-- Easy forms of arbitrary bit rotate.
-- Usually there are some instructions to be saved.
-- shlqBits, rotqBits :: (SPUType a) => VR a -> VR a -> VR a
-- shlqBitsi, rotqBitsi :: (SPUType a) => VR a -> Integer -> VR a

{- Not used anywhere -}
-- shlqBits v bits = shlqbi (shlqbybi v bits) bits
-- rotqBits v bits = rotqbi (rotqbybi v bits) bits
-- shlqBitsi v bits = shlqbii (shlqbyi v (bits `div` 8)) bits
-- rotqBitsi v bits = rotqbii (rotqbyi v (bits `div` 8)) bits

-- Code for Handling Pairs of Doubles

-- The functions |interleaveHigh| and |interleaveLow|
-- treat their argument and result vectors
-- as vectors of four 32-bit words;
-- |interleaveHigh| implements
-- |\ u v -> [u !! 0, v !! 0, u !! 2, v !! 2]|.
-- This is a SIMD instruction for 64-bit double-words,
-- which is made explicit by the use of |shufSpecDup|:

pMapoPoP :: (t1 -> t2 -> t3 -> t4 -> b) -> t1 -> (t2, t2) -> t3 -> (t4, t4) -> (b, b)
pMapoPoP f w (x1,x2) y (z1,z2)  = (f w x1 y z1, f w x2 y z2)
pMapPPPP :: (t1 -> t2 -> t3 -> t4 -> b) -> (t1, t1) -> (t2, t2) -> (t3, t3) -> (t4, t4) -> (b, b)
pMapPPPP f (w1,w2) (x1,x2) (y1,y2) (z1,z2)  = (f w1 x1 y1 z1, f w2 x2 y2 z2)
pMapPPoP :: (t1 -> t2 -> t3 -> t4 -> b) -> (t1, t1) -> (t2, t2) -> t3 -> (t4, t4) -> (b, b)
pMapPPoP f (w1,w2) (x1,x2) y (z1,z2)  = (f w1 x1 y z1, f w2 x2 y z2)
pMapPPP :: (t1 -> t2 -> t3 -> b) -> (t1, t1) -> (t2, t2) -> (t3, t3) -> (b, b)
pMapPPP f (x1,x2) (y1,y2) (z1,z2)  = (f x1 y1 z1, f x2 y2 z2)
pMapPoP :: (t1 -> t2 -> t3 -> b) -> (t1, t1) -> t2 -> (t3, t3) -> (b, b)
pMapPoP f (x1,x2) y (z1,z2)        = (f x1 y z1, f x2 y z2)
pMapooP :: (t1 -> t2 -> t3 -> b) -> t1 -> t2 -> (t3, t3) -> (b, b)
pMapooP f x y (z1,z2)              = (f x y z1, f x y z2)
pMapoPP :: (t1 -> t2 -> t3 -> b) -> t1 -> (t2, t2) -> (t3, t3) -> (b, b)
pMapoPP f x (y1,y2) (z1,z2)        = (f x y1 z1, f x y2 z2)
pMapPoo :: (t1 -> t2 -> t3 -> b) -> (t1, t1) -> t2 -> t3 -> (b, b)
pMapPoo f (x1,x2) y z              = (f x1 y z, f x2 y z)
pMapPPo :: (t1 -> t2 -> t3 -> b)
                 -> (t1, t1) -> (t2, t2) -> t3 -> (b, b)
pMapPPo f (x1,x2) (y1,y2) z        = (f x1 y1 z, f x2 y2 z)
pMapoPo :: (t1 -> t2 -> t3 -> b) -> t1 -> (t2, t2) -> t3 -> (b, b)
pMapoPo f x (y1,y2) z              = (f x y1 z, f x y2 z)

pMap :: (t -> b) -> (t, t) -> (b, b)
pMap f (x1,x2) = (f x1, f x2)

pMapPP :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b)
pMapPP f (x1,x2) (y1,y2)  = (f x1 y1, f x2 y2)
pMapPo :: (t1 -> t2 -> b) -> (t1, t1) -> t2 -> (b, b)
pMapPo f (x1,x2) y  = (f x1 y, f x2 y)
pMapoP :: (t1 -> t2 -> b) -> t1 -> (t2, t2) -> (b, b)
pMapoP f x (y1,y2)  = (f x y1, f x y2)

pFoldoP :: (a1 -> a2 -> b -> c) -> a1 -> (a2, b) -> c
pFoldoP f = uncurry . f

wrd2P :: PowerISA repr => repr VR -> (repr VR, repr VR)
wrd2P v = (shufB v v  [0,1,2,3,      0x80, 0x80, 0x80, 0x80
                      ,8,9,10,11,    0x80, 0x80, 0x80, 0x80  :: Word8
                      ]
          ,shufB v v  [4,5,6,7,      0x80, 0x80, 0x80, 0x80
                      ,12,13,14,15,  0x80, 0x80, 0x80, 0x80  :: Word8
                      ])

copySignFlt :: Interp VR -> Interp VR -> Interp VR
copySignFlt from to = xxsel to from signBitFlt
signBitFlt :: Interp VR
signBitFlt = unwrds4 @Interp $ (2147483648 :: Word32)-- 2^31
copySignDbl :: Interp VR -> Interp VR -> Interp VR
copySignDbl from to = xxsel to from signBitDbl
signBitDbl :: Interp VR
signBitDbl = undwrds2 @Interp $ (9223372036854775808 :: Word64) -- 2^63
nanDbl :: Interp VR
nanDbl = undwrds2 @Interp (0x7ff8000000000000 :: Word64)
infDbl :: Interp VR
infDbl = undwrds2 @Interp (0x7ff0000000000000 :: Word64)
maxDbl :: Interp VR
maxDbl = unwrds @Interp [0x7fefffff,0xffffffff,0x7fefffff,0xffffffff :: Word32]
zero :: Interp VR
zero = uninteger @Interp 0

-- Rotate left by a number of bits
-- TODO delete (improperly translated from SPU)
-- rotLeftOdd :: PowerISA repr => Word8 -> repr VR -> repr VR -> repr VR
-- rotLeftOdd shft v = vrlq (vrlq v bitShiftVector)
--   where
--     posShift = mod shft 128
--     (_byteShift,bitShift) = divMod posShift 8
--     bitShiftVector = unbytes([0,0,0,0,0,0,0,bitShift,0,0,0,0,0,0,0,0])
    -- byteShiftInBits = 8*byteShift
    -- byteShiftVector = unbytes @Interp ([0,0,0,0,0,0,0,byteShiftInBits])

-- Play with Memory-Based Lookup
-- Test for agreement between list length and number of bits used
-- for memory based lookups.
bitsMatch :: (Integral b, Foldable t1, Show b) => (b, b) -> t2 -> t1 a -> [Char] -> t3 -> t4
bitsMatch (high,low) sze lst errMsg result
  =  if 2 ^ (high - low + 1) == length lst
     then listMatch sze lst errMsg result
     else error  $ errMsg ++ " bitrange " ++ show (high,low)
                 ++ " doesn't match list length " ++ show (length lst)
                

-- Used for double precision but not needed right now 
-- Efficiently look up in a list based on bits with defined positions.
-- lookupWrds2Dbls :: forall a n . (NatE n, HasJoin String (VR a), SPUType a) => (Int,Int) -> [Double] -> n -> VR a -> (VR a,VR a)
-- lookupWrds2Dbls (high,low) lst size v =  bitsMatch (high,low) size lst "MathUtils.lookupWrds2Dbls"
--                                          (mergeDbls w1a w1b, mergeDbls w2a w2b)
--   where
--     cm = initLSMR "lookupWrds2Dbls" size (map undoubles2 lst)
--     [w1a,w1b,w2a,w2b] = map (fst . flip lqxMR cm) addrs :: [repr VR]
--     addrs = [idxToQW, vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*8]), vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*4]), vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*12])] -- multiply by 8 because rotqbyi is byte rotate and vrlq is bit rotate
--     idxToQW = vrlq justIdx (unbytes[0,0,0,0,0,0,0,4]) -- * 16
--     (masked,b1,b2,b3,b4) = case (high,low) of
--              (7,0) -> (v,3,7,11,15)
--              (6,0) -> (xxland v $ undwrds2 $ (127 :: Integer),3,7,11,15)
--              (19,12) -> (vrlq v (unbytes[0,0,0,0,0,0,0,4]),1,5,9,13)
--              _ -> error $ "MathUtils.lookupWrds2Dbls undefined "++show (high,low)
--     justIdx = xxperm masked masked $ unbytes . map (\ x -> x :: Integer) $  [0x80, 0x80, 0x80, b1
--                                              ,0x80, 0x80, 0x80, b2
--                                              ,0x80, 0x80, 0x80, b3
--                                              ,0x80, 0x80, 0x80, b4
--                                              ]

-- Used for double precision but not needed right now 
-- lookupWrds2Quads :: forall a n . (NatE n, HasJoin String (VR a), SPUType a) => (Int,Int) -> [Integer] -> n -> VR a -> (VR a, VR a)
-- lookupWrds2Quads (high,low) lst size v =  bitsMatch (high,low) size lst "MathUtils.lookupWrds2Dbls"
--                                          (mergeDbls w1a w1b, mergeDbls w2a w2b)
--   where
--     cm = initLSMR "lookupWrds2Quads" size (map undwrds2 lst)
--     [w1a,w1b,w2a,w2b] = map (fst . flip lqxMR cm) addrs :: [repr VR]
--     addrs =  [         idxToQW
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*8]) --multiply by 8 because vrlq is a bit shift and this was a byte shift before
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*4])
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*12])
--              ]
--     idxToQW = vrlq justIdx (unbytes[0,0,0,0,0,0,0,4]) -- * 16
--     (masked,b1,b2,b3,b4) = case (high,low) of
--              (7,0) -> (v,3,7,11,15)
--              (6,0) -> (xxland v $ undwrds2 $ (127 :: Integer),3,7,11,15)
--              (19,12) -> (vrlq v (unbytes[0,0,0,0,0,0,0,4]),1,5,9,13)
--              _ -> error $ "MathUtils.lookupWrds2Dbls undefined "++show (high,low)
--     justIdx = xxperm masked masked $ unbytes . map (\ x -> x :: Integer) $  [0x80, 0x80, 0x80, b1
--                                              ,0x80, 0x80, 0x80, b2
--                                              ,0x80, 0x80, 0x80, b3
--                                              ,0x80, 0x80, 0x80, b4
--                                              ]

-- Addresses should usually be prepared in the prologue takes four words packed in register

-- Lots of patterns to mine here.
-- order of packing needs to match packing and unpacking

-- lists of lists of length 2
-- produces pair of lists of length 2

-- So should be pairs!!!

-- Used for double precision but not needed right now 
-- lookupWrdsPairDbls :: forall a n . (NatE n, HasJoin String (VR a), SPUType a)   => (Int,Int) -> [[Double]] -> n -> VR a -> [(VR a, VR a)]
-- lookupWrdsPairDbls (high,low) lst size v
--     =  bitsMatch (high,low) size lst "MathUtils.lookupWrdsPairDbls"
--        $ zip [mergeLeftDbls w1a w1b, mergeRightDbls w1a w1b]
--              [mergeLeftDbls w2a w2b, mergeRightDbls w2a w2b]
--   where
--     cm = initLSMR "lookupWrdsPairDbls" size (map undoubles lst)
--     [w1a,w1b,w2a,w2b] = map (fst . flip lqxMR cm) addrs
--     addrs =  [         idxToQW
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*8]) --multiply by 8 because rotqbyi takes bytes but vrlq is a bit rotate
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,4*8])
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,12*8])
--              ]
--     idxToQW = vrlq justIdx (unbytes[0,0,0,0,0,0,0,4]) -- * 16
--     (masked,b1,b2,b3,b4) = case (high,low) of
--              (7,0) -> (v,3,7,11,15)
--              (6,0) -> (xxland v $ undwrds2 $ (127 :: Integer),3,7,11,15)
--              (19,12) -> (vrlq v (unbytes[0,0,0,0,0,0,0,4]),1,5,9,13)
--              _ -> error $ "MathUtils.lookupWrdsPairDbls undefined "++show (high,low)
--     justIdx = xxperm masked masked $ unbytes . map (\ x -> x :: Integer) $  [0x80, 0x80, 0x80, b1
--                                              ,0x80, 0x80, 0x80, b2
--                                              ,0x80, 0x80, 0x80, b3
--                                              ,0x80, 0x80, 0x80, b4
--                                              ]

-- Used for double precision but not needed right now 
-- lookupWrdsPairVals :: forall a n . (NatE n, HasJoin String (VR a), SPUType a) => (Int,Int) -> [[Integer]] -> n -> VR a -> [(VR a, VR a)]
-- lookupWrdsPairVals (high,low) lst size v
--     =  bitsMatch (high,low) size lst "MathUtils.lookupWrdsPairVals"
--        $ zip [mergeLeftDbls w1a w1b, mergeRightDbls w1a w1b]
--              [mergeLeftDbls w2a w2b, mergeRightDbls w2a w2b]
--   where
--     cm = initLSMR "lookupWrdsPairVals" size (map undwrds lst)
--     [w1a,w1b,w2a,w2b] = map (fst . flip lqxMR cm) addrs
--     addrs =  [         idxToQW
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*8]) --multiply by 8 because rotqbyi took bytes and vrlq is a bit rotate 
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,4*8])
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,12*8])
--              ]
--     idxToQW = vrlq justIdx (unbytes[0,0,0,0,0,0,0,4]) -- * 16  \edcomm{CKA}{This rotqbii should be merged with the one in the case.}
--     (masked,bs) = case (high,low) of
--              (7,0) -> (v,[3])
--              (6,0) -> (xxland v $ (uninteger (127 :: Integer)),[3]) --3,7,11,15)
--              (19,8) -> (xxland v $ unwrds4 (0xfff00 :: Integer),[1,2]) --1,2,4,5,8,9,12,13)
--              (19,9) -> (xxland (vrlw v (unbytes [0,0,0,(-1),0,0,0,0])) $ unwrds4 (0x7ff00 :: Integer),[1,2]) --1,2,4,5,8,9,12,13)
--              (19,12) -> (vrlq v (unbytes[0,0,0,0,0,0,0,4]),[1]) --1,5,9,13)
--              (19,14) -> (xxland (vrlq v (unbytes[0,0,0,0,0,0,0,2])) $ uninteger (64), [1])
--              (19,15) -> (xxland (vrlq v (unbytes[0,0,0,0,0,0,0,1])) $ uninteger (32), [1])
--              (20,8) -> (xxland v $ unwrds4 (0x1fff00 :: Integer),[1,2]) --1,2,4,5,8,9,12,13)
--              _ -> error $ "MathUtils.lookupWrdsPairVals undefined "++show (high,low)
--     justIdx = xxperm masked masked $ unbytes $ map (\ x -> x :: Integer) $ concat $ take 4 $ iterate (map nextWord) b4

--     b4 =  reverse $ take 4 $ reverse $ [0x80, 0x80, 0x80] ++ bs

--     nextWord x =  if x == 0x80 then 0x80
--                   else x + 4

-- Used for double precision but not needed right now 
-- lookupInPlacePairVals :: forall a n .
  -- (NatE n, HasJoin String (VR a), SPUType a) => [[Integer]] -> n -> VR a -> [(VR a, VR a)]
-- lookupInPlacePairVals lst size idxToQW
--     =  bitsMatch (log2Int $ length lst,1::Integer) size lst "MathUtils.lookupWrdsPairVals"
--        $ zip [mergeLeftDbls w1a w1b, mergeRightDbls w1a w1b]
--              [mergeLeftDbls w2a w2b, mergeRightDbls w2a w2b]
--   where
--     cm = initLSMR "lookupWrdsPairVals" size (map undwrds lst)
--     [w1a,w1b,w2a,w2b] = map (fst . flip lqxMR cm) addrs
--     addrs =  [         idxToQW
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8*8]) -- multiply by 8 because rotqbyi was a byte rotate and vrlq is a bit rotate 
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,4*8])
--              , vrlq idxToQW (unbytes[0,0,0,0,0,0,0,12*8])
--              ]

-- Lookup values

-- Used for double precision but not needed right now 
-- Takes the address offset from the bits $2^15 ... 2^4$, ignoring $2^3...2^0$ and ignoring $2^31...2^16$ (bits with word values).
-- lookupInPlaceVals :: forall a n . (NatE n, HasJoin String (VR a), SPUType a) => [[Integer]] -> n -> VR a -> [VR a]
-- lookupInPlaceVals lst size v
--     =  [mergeLeftDbls w1a w1b, mergeRightDbls w1a w1b]
--   where
--     cm = initLSMR "lookupWrdsPairVals" size (map undwrds lst)
--     [w1a,w1b] = map (fst . flip lqxMR cm) addrs
--     addrs = [idxToQW, vrlq idxToQW (unbytes[0,0,0,0,0,0,0,8])]
--     idxToQW = shufB1 v  [0x80,0x80,2::Integer,3::Integer,    0x80,0x80,0x80,0x80
--                         ,0x80,0x80,10::Integer,11::Integer,  0x80,0x80,0x80,0x80]

-- Add correctness check on high,low.
-- lookupWordsPair4 :: (SPUType a) => (Integer,Integer) -> [Integer] -> VR a -> (VR a, VR a)
lookupWordsPair4 :: (Integral a, PowerISA repr) => (a, Word8) -> [Word64] -> repr VR -> (repr VR, repr VR)
lookupWordsPair4 (high,low) [qw1,qw2,qw3,qw4] v = (lkup key1, lkup key2)
  where
    bs = concatMap (bytes @Interp . undwrds) [[qw1,qw2], [qw3,qw4]]
    (b1s,b2s) = splitAt 16 $ map snd $ List.sort $ zip lexOrd bs
    lkup = vperm (unbytes b1s) (unbytes b2s)

    (v',byt,bts) = case (low `mod` 8) of
        0 -> (v, 3 - low `div` 8, (4 :: Word8))
        1 -> (v, 3 - low `div` 8, (6 :: Word8))
        2 -> (v, 3 - low `div` 8, (12 :: Word8))
        3 -> (v, 3 - low `div` 8, (24 :: Word8))
        lw -> (vrlq v (unbytes[0,0,0,0,0,0,0,(8-lw)]), (2 - lw `div` 8), 4)

    key1' = shufB1 v' $ map ((flip mod 16) . (+byt))           $ concat [replicate 8 i | i <- ([0,8] :: [Word8])]
    key2' = shufB1 v' $ map ((+16) . (flip mod 16) . (+byt))  $ concat [replicate 8 i | i <- ([4,12] :: [Word8])]

    key1 = xxsel byteWithinDbl key1' bitMask
    key2 = xxsel byteWithinDbl key2' bitMask

    bitMask = unbytes16 $ bts
    byteWithinDbl = unbytes $ map snd $ take 16 $ lexOrd

    lexOrd = case (high `mod` 8) of
        2  -> [((b1,b2,oct,b3),b3+8*b2+16*b1)  |  oct <- ([0..3] :: [Word8]), b1<-([0,1] :: [Word8]),b2<-([0,1] :: [Word8]),b3<-([0,1] :: [Word8])]
        3  -> [((b1,oct,b2,b3),b3+2*b2+16*b1)  |  oct <- ([0..3] :: [Word8]), b1<-([0,1] :: [Word8]),b2<-([0,1] :: [Word8]),b3<-([0,1] :: [Word8])]
        4  -> [((oct,b1,b2,b3),b3+2*b2+4*b1)  |  oct <- ([0..3] :: [Word8]), b1<-([0,1] :: [Word8]),b2<-([0,1] :: [Word8]),b3<-([0,1] :: [Word8])]
        _  -> [((b1,b2,b3,oct),b3*4+b2*8+16*b1)  | oct <- ([0..3] :: [Word8]), b1<-([0,1] :: [Word8]),b2<-([0,1] :: [Word8]),b3<-([0,1] :: [Word8])]

lookupWordsPair4 (_high,_lw) x _ = error $ "MathUtils.lookupWordsPair4 length " ++ show (length x)

-- Wrapper for merging three doubles into one 128-bit vector to save table space,
-- and for extracting the three components.  The first two extractions are
-- optional, if the low-order bits of the first two operands do not matter.

-- Preconditions? Trade-offs? Precision? Method?

-- mergeSplit3Quads :: (SPUType a) => (Int,Int) -> Int
  -- ->  ([[Integer]] -> [Integer] -> [[Integer]]
  --     ,([VR a]->VR a,[VR a]->VR a,[VR a]->VR a))
mergeSplit3Quads :: (PowerISA repr) => (Int,Int) -> Int -> ([[Word64]] -> [Word64] -> [[Word64]],([repr VR]->repr VR,[repr VR]->repr VR,[repr VR]->repr VR))
mergeSplit3Quads (bytes1,bytes2) sndBytes
  =  if bytes1 + bytes2 + sndBytes > 16
       then error $ "MathUtils.mergeSplit3Quads "++show (bytes1,bytes2,sndBytes)
       else (\ lstA lstB -> zipWith munge lstA lstB,(extract1,extract2,extract3))
  where
    extract1 [x,y] = shufB x y  ([0..b1-1]++(zeros bytes1)
                                ++[16..15+b1]++(zeros bytes1))
    extract1 x = error $ "MathUtils.mergeSplit3Quads e1" ++ (show $ length x)
    extract2 [x,y] = shufB x y  ([8..7+b2]++(zeros bytes2)
                                ++[24..23+b2]++(zeros bytes2))
    extract2 x = error $ "MathUtils.mergeSplit3Quads e2" ++ (show $ length x)
    extract3 [x,y] = shufB x y  ((take sndBytes $ [b1..7]++[8+b2..15]) ++ (zeros sndBytes)
                                ++(take sndBytes $ [16+b1..23]++[24+b2..31]) ++(zeros sndBytes))
    extract3 x = error $ "MathUtils.mergeSplit3Quads e3" ++ (show $ length x)

    b1 = (fromIntegral bytes1) :: Word8
    b2 = (fromIntegral bytes1) :: Word8

    zeros x = (replicate (8-x) 0x80) :: [Word8]
    
    munge :: [Word64] -> Word64 -> [Word64]
    munge a b = dwrds @Interp $ shufB (undwrds a) (undwrds2 b) $ ((map fromIntegral $ [0..bytes1-1] ++ [16..23-bytes1] ++ [8..7+bytes2] ++ [24-bytes1..31-bytes1-bytes2]) :: [Word8])

-- Collect the high words of doubles from two vector registers into one vector register.
-- mergeDbls :: (SPUType a, HasJoin String (VR a)) => VR a -> VR a -> VR a
mergeDbls :: PowerISA repr => repr VR -> repr VR -> repr VR
mergeDbls a b = xxsel a b $ unwrds ([0, 0, fromIntegral (-1 :: Integer), fromIntegral (-1 :: Integer)] :: [Word32])


  {- equivJoin "can do this in either unit" "unknown"
  [xxperm' ([shufSelLeft64 0] ++ shufSpecFlip [shufSelLeft64 1]) a b
  ,xxsel a b $ unwrds ([0,0,-1,-1] :: [Integer])
  ]-}

dblExpBits :: PowerISA repr => repr VR
dblExpBits = (unbytes $ (\ x-> x ++ x) $ ([0x7f,0xf0,0,0, 0,0,0,0] :: [Word8]))

dblExpBitsInWord :: PowerISA repr => repr VR
dblExpBitsInWord = unwrds4 (0x7ff00000 :: Word32)

-- The |mkDbl*| functions are only exported, not used here in |MathUtils|.

-- Make |xxperm| key to imitate |xxsel|.
-- mkDblUpDownKeyBool :: (SPUType a) => VR a -> VR a
mkDblUpDownKeyBool :: PowerISA repr => repr VR -> repr VR
mkDblUpDownKeyBool v = xxsel (unbytes ([0..15] :: [Word8])) v (unbytes16 (0x10 :: Word8))

-- Make a |xxperm| key from the specified bit (bit endian) of a double to select between a first and second argument.
-- This saves even instructions even for a single |xxsel|.

-- mkDblUpDownKey :: (SPUType a) => Int -> VR a -> VR a
{-# LANGUAGE ScopedTypeVariables #-}

mkDblUpDownKey :: forall a repr. PowerISA repr => a -> repr VR -> repr VR
mkDblUpDownKey _bt v = key
  where
    (byt,bt) = divMod bt 8
    sixteenTopBytes = shufB1 v $
      map (fromIntegral (byt :: Integer) +) $ if (bt :: Integer) < 3
          then ([8,  0,0,0,0,0,0,0,0, 8,8,8,8,8,8,8] :: [Word8])
          else ([    0,0,0,0,0,0,0,0, 8,8,8,8,8,8,8,8] :: [Word8])
    signBits = (if (bt :: Integer) == 3 then id else flip vrlq (unbytes [0,0,0,0,0,0,0, (mod (8 + 5 + bt) 8 :: Word8)])) sixteenTopBytes
    key = xxsel (unbytes ([0..15] :: [Word8])) signBits (unbytes16 (0x10 :: Word8))


-- Make a |xxperm| key from the sign bit of a double to select between a first and second argument.
-- This saves even instructions even for a single |xxsel|.
-- mkDblSignUpDownKey :: (SPUType a) => VR a -> VR a
mkDblSignUpDownKey :: PowerISA repr => repr VR -> repr VR
mkDblSignUpDownKey v = key
  where
    sixteenTopBytes = shufB1 v ([8, 0,0,0,0,0,0,0,0, 8,8,8,8,8,8,8] :: [Word8])
    signBits = vrlq sixteenTopBytes (unbytes[0,0,0,0,0,0,0,5])
    key = xxsel (unbytes ([0..15] :: [Word8])) signBits (unbytes16 (0x10 :: Word8))

-- Make a |xxperm| key from the sign bit of a double to select between the first (L) and second (R) word8.
-- This saves even instructions even for a single |xxsel|.

-- mkDblSignLRKey :: (SPUType a) => VR a -> VR a
mkDblSignLRKey :: PowerISA repr => repr VR -> repr VR
mkDblSignLRKey v = key
  where
    sixteenTopBytes = shufB1 v ([8, 0,0,0,0,0,0,0,0, 8,8,8,8,8,8,8] :: [Word8])
    signBits = vrlq sixteenTopBytes (unbytes[0,0,0,0,0,0,0,4])
    key = xxsel (unbytes ([0..15] :: [Word8])) signBits (unbytes16 (0x8 :: Word8))

{- Horner Evaluation -}
-- Horner's rule:
-- \[
--   \sum_{i=0}^{n} a_i x = a_0 + x \left( a_1 + x \left( a_2 + ...
--   x a_n\right) ... \right)
-- \]
-- can be used to evaluate a polynomial in $n$ fused multiply-adds.
-- With SIMD instructions one can compute four single-precision
-- values in parallel using a series of |xvmaddmsp|s.
-- The function |hornerV| does this, and works for any order polynomial.
-- The order of the polynomial is one minus the length of the list of coefficients.
-- This is the highest-throughput way of evaluating polynomials.
-- Other methods can reduce latency by increasing parallelism,
-- but this is wasteful for deeply software-pipelined loops.

hornerGen :: (a -> b -> b -> b) -> [b] -> a -> b
hornerGen multAdd   (a1:a2:as) v = multAdd v (hornerGen multAdd (a2:as) v) a1
hornerGen _multAdd  [a1] _ = a1
hornerGen _multAdd  [] _ = error "hornerGen: need a coeff"

-- Old type def
-- hornerV :: SPUType a => [VR a] -> VR a -> VR a
hornerV :: forall repr. PowerISA repr => [repr VR] -> repr VR -> repr VR
hornerV = hornerGen xvmaddmsp

-- Old type def
-- hornerVDbl :: SPUType a => [VR a] -> VR a -> VR a
hornerVDbl :: forall repr. PowerISA repr => [repr VR] -> repr VR -> repr VR
hornerVDbl = hornerGen xvmaddmdp

hornerPDbl :: forall repr. PowerISA repr => [(repr VR,repr VR)] -> (repr VR,repr VR) -> (repr VR,repr VR)
hornerPDbl = hornerGen (pMapPPP xvmaddmdp)

-- The arguments to a |horner*| function are just a polynomial,
-- and since we will want to operate also more abstractly on polynomials,
-- we define a special-purpose datatype:

data Polynomial v c = Polynomial
  {polyVar :: v
  ,polyCoeffs :: [c]
  }

-- |hornerGen| is easily adapted:

polyHorner :: (v -> c -> c -> c) -> Polynomial v c -> c
polyHorner multAdd p = hornerGen multAdd (polyCoeffs p) (polyVar p)

-- Since in all typical application cases, |v = c|,
-- we do not go to extra effort to keep the two apart.

-- for |polyFudge|: the two coefficient lists have the same length.
-- Just for safety, we check this at run-time:
polyFudge :: (a -> b -> c) -> Polynomial a a -> Polynomial b b -> Polynomial c c
polyFudge f (Polynomial pVar pCoeffs) (Polynomial qVar qCoeffs) = Polynomial
  {polyVar = pVar `f` qVar
  ,polyCoeffs  =  assert (pLen == qLen)
                  (unwords [ "polyFudge: ceofficient lists have lengths"
                           , show pLen, "and", show qLen])
               $  zipWith f pCoeffs qCoeffs
  }
  where
    pLen = length pCoeffs
    qLen = length qCoeffs

-- In |(p', r) = polyFudge' p q|,
-- |p'| has the same length as |p|,
-- and |q| is either filled up with zeroes \edcomm{WK}{at the right end?}
-- or split (in the right direction?},
-- and the rest is returned as |r|.
polyFudge' :: b -> (a -> b -> c) -> Polynomial a a -> Polynomial b b -> (Polynomial c c, [b])
polyFudge' zeroB f (Polynomial pVar pCoeffs) (Polynomial qVar qCoeffs) = let
    pLen = length pCoeffs
    qLen = length qCoeffs
    qFill = replicate (qLen - pLen) zeroB ++ qCoeffs
    (revPairs, qRest) = zipShorterWithRest (reverse pCoeffs) (reverse qFill)
  in  (Polynomial
         {polyVar = pVar `f` qVar
         ,polyCoeffs = reverse $ map (uncurry f) revPairs
         }
      ,reverse qRest
      )

-- Function for turning a list of coefficients
-- into a series of multiply-adds to evaluate a polynomial:
-- $$
--     [a0,a1,a2]
--   \mapsto
--     \left( x \mapsto (a0 + x ( a1 + x a2) ), ...\right)
-- $$

-- horner4 :: SPUType a => [Double] -> VR a -> VR a
horner4 :: PowerISA repr => [Double] -> repr VR -> repr VR
horner4 (a1:a2:a3:as)  x = xvmaddmsp x (horner4 (a2:a3:as) x)  (unfloats4 a1)
horner4 [a1,a2]        x = xvmaddmsp x (unfloats4 a2)          (unfloats4 a1)
horner4 _ _ = error "horner4 shouldn't get called with less than two elements"

-- horner2Dbl :: SPUType a => [Double] -> VR a -> VR a
horner2Dbl :: PowerISA repr => [Double] -> repr VR -> repr VR
horner2Dbl (a1:a2:a3:as)  x = xvmaddmdp x (horner2Dbl (a2:a3:as) x)  (undoubles2 a1)
horner2Dbl [a1,a2]        x = xvmaddmdp x (undoubles2 a2)            (undoubles2 a1)
horner2Dbl _ _ = error "horner2Dbl shouldn't get called with less than two elements"

-- Operate on 2 doubles in parallel, coefficients specified as int64s.
-- horner2w64 :: SPUType a => [Integer] -> VR a -> VR a
horner2w64 :: PowerISA repr => [Word64] -> repr VR -> repr VR
horner2w64 (a1:a2:a3:as)  x = xvmaddmdp x (horner2w64 (a2:a3:as) x)  (undwrds2 a1)
horner2w64 [a1,a2]        x = xvmaddmdp x (undwrds2 a2)            (undwrds2 a1)
horner2w64 _ _ = error "horner2w64 shouldn't get called with less than two elements"

-- Same for different polynomials:
-- horner :: SPUType a => [Double] -> [Double] -> [Double] -> [Double] -> VR a -> VR a
horner :: PowerISA repr => [Double] -> [Double] -> [Double] -> [Double] -> repr VR -> repr VR
horner (a1:a2:a3:as) (b1:b2:b3:bs) (c1:c2:c3:cs) (d1:d2:d3:ds) x
  = xvmaddmsp x (horner (a2:a3:as) (b2:b3:bs) (c2:c3:cs) (d2:d3:ds) x) $ unfloats [a1,b1,c1,d1]
horner (a1:a2:[]) (b1:b2:[]) (c1:c2:[]) (d1:d2:[]) x
    = xvmaddmsp x (unfloats [a2,b2,c2,d2]) (unfloats [a1,b1,c1,d1])
horner _ _ _ _ _ = error "horner shouldn't get called with less than two elements, or elements without 4 elements"

-- hornerDbl :: SPUType a => [Double] -> [Double] -> VR a -> VR a
hornerDbl :: PowerISA repr => [Double] -> [Double] -> repr VR -> repr VR
hornerDbl (a1:a2:a3:as) (b1:b2:b3:bs) x
  = xvmaddmdp x (hornerDbl (a2:a3:as) (b2:b3:bs) x) $ undoubles [a1,b1]
hornerDbl (a1:a2:[]) (b1:b2:[]) x
    = xvmaddmdp x (undoubles [a2,b2]) (undoubles [a1,b1])
hornerDbl _ _ _ = error "hornerDbl shouldn't get called with less than two elements, or elements without 4 elements"

{- Mantissa Extraction -}

-- An IEEE floating point number is stored as packed bit fields:
-- $1$ sign bit, $8$ exponent bits and $23$ mantissa bits
-- (for single precision).
-- The SPU ISA uses the same format, although exceptional values
-- are not recognized.
-- So on the SPU all values of the exponent and mantissa bit fields
-- represent the floating point number
-- \begin{equation}
-- x = (-1)^{\text{sign}}2^{\text{exponent}-127}1.\text{mantissa}.
-- \end{equation}

-- Common patterns in special function evaluation
-- include extraction of the exponent and mantissa fields.
-- Since the exponent bits are often used in different
-- bit positions, we have higher-level patterns which
-- construct declarative assembly and the various constants
-- required to put the exponential bits into any position
-- in the word.

-- The fractional part represented by the mantissa is
-- most often used as a floating point number,
-- so we have a pattern to form the number
-- $1.$mantissa$\in [1,2)$, by merging the low-order |bits| mantissa bits
-- with the bit pattern for $1.0$.

onePlusMant :: PowerISA repr => Integer -> repr VR -> repr VR
onePlusMant bts v = xxsel floatOne v mantisaBits
  where
    floatOne = unwrds4 0x3f800000
    mantisaBits = unwrds4 $ (2^bts - 1)


{- Exponent Extraction -}
-- Depending on the use, extracting the exponent can be
-- combined with other operations,
-- but we provide an odd-pipeline method of
-- putting the exponent byte into any aligned byte surrounded
-- by zeros.
-- This works by rotating the whole quadword left one bit,
-- to rotate out the mantissa's sign bit and put the exponent byte
-- into the first byte slot of each word.
-- Then use |xxperm| to insert that byte into the desired
-- position slot of the word which is otherwise filled with zeros.

-- extractExp :: SPUType a => Integer -> VR a -> VR a
extractExp :: (PowerISA repr, Eq a, Num a) => a -> repr VR -> repr VR
extractExp destSlot v =
    shufB1 vShifted $ mapWord destSlot =<< ([0..3] :: [Word8])
  where
    vShifted = vrlq v (unbytes[0,0,0,0,0,0,0,1])

mapWord :: (Eq a1, Num a1, Num a2) => a1 -> a2 -> [a2]
mapWord 0 i = [0 + 4 * i, 0x80, 0x80, 0x80]
mapWord 1 i = [0x80, 0 + 4 * i, 0x80, 0x80]
mapWord 2 i = [0x80, 0x80, 0 + 4 * i, 0x80]
mapWord 3 i = [0x80, 0x80, 0x80, 0 + 4 * i]
mapWord _ _ = error "extractExp: mapWord: not a slot"

mapword :: Num a => Int -> a -> [a]
mapword k i =  let (xs, ys) = splitAt k $ replicate 3 0x80
               in xs ++ 4 * i : ys

{- Calculating a Bit-Shifted Division -}

-- In integer arithmetic,
-- we can save considerable numbers of instructions
-- by using \emph{approximate inverses}
-- instead of converting numbers to floating point,
-- doing expensive arithmetic, and converting back.

-- Some of the most common uses
-- are captured by a pattern that approximates the calculation of the result of
-- the division by $q$ of the integer-coefficient linear form $p \cdot x + s$
-- in such a way that the integral part of the
-- quotient $\frac{p \cdot x + s}{q}$ is returned in the left $(32-n)$ bits,
-- and the fractional part in the right $n$ bits.


-- Given a precision $n$, this calculates
-- for each word element $v_i$ of the vector |v|,
-- the approximate linear function
-- \[
-- \frac{p v_i + s}{q} \approx_{\text{fixed point}(n)}
-- \floor{2^n\left(\frac{p}{q}\cdot v_i +\frac{s}{q}\right)}
-- \enskip.
-- \]
-- Since $p$, $q$, and $s$ are compile-time integer constants,
-- we can approximate this using a linear form with coefficients
-- calculated at compile-time.
-- As a result of this approach,
-- the result has only up to $16$ correct digits
-- corresponding to the integer part (starting at the $2^n$ bit)
-- and a number of correct fractional digits to the right
-- of this point.
-- The number of correct digits decreases with the size of
-- the inputs.

-- If fractional digits are needed,
-- it is easier to enumerate all possible inputs than to
-- formally reason about the achieved precision;
-- we show an example of this
-- in our cube root implementation in \sectref{Cbrt}.

-- In the implementation, we have to
-- check to see whether a nonzero offset is required.
-- If it is, we need to use a fused multiply add, |mpya|,
-- and two register constants;
-- if not, we can use the immediate form if the multiplicand is
-- in the limited range for signed or unsigned immediates,
-- otherwise we use the register form with a single register constant.

-- divShiftMA :: SPUType a
  --  => Integer -> Integer -> Integer -> Integer
  --  -> VR a -> VR a
-- divShiftMA _p 0 _s _n _v        = error "MathUtils.div by 0"
-- divShiftMA p q s n v = if s /= 0 then vaddsws (vmulosh m v) b else if m' < 1024  && m' > 0 then vmulosh v (unwrds4 m') else if m' < 512  && m' >= -512 then vmulosh v (unwrds4 m') else vmulosh v m
--   where       -- using integer exponent and division
--     m' = (p * 2^n + (q-1)) `div` q
--     m = unwrds4 m'
--     b = unwrds4 $ (s * 2^n) `div` q

-- By encapsulating all these relatively tricky details
-- into a DSL function,
-- we free the domain expert from doing all this low-level arithmetic
-- and allow them to work on a higher level of abstraction,
-- as we will show in \sectref{Cbrt}.

{- Mixed log/linear intervals -}

-- We can do lookups based on intervals which mix logarithmic and linear scales
-- by forming an index from a combination of bits from the exponent field and from the mantissa.
-- This is an efficient way to construct lookup keys corresponding
-- to a contiguous set of intervals,
-- with either equal-sized intervals or intervals whose size
-- doubles regularly from one end to the other.
-- Varying the size of the intervals may result in an approximation
-- with lower order but the same maximum error,
-- especially
-- for functions with singularities or zero crossings.

-- However we choose the interval domains of the polynomial
-- approximations, we have to look up the correct set of
-- coefficients at run time.
-- On SIMD architectures with byte permutation, like
-- VMX and the SPU ISA,
-- this can be done efficiently for some sizes of tables
-- entirely in registers without accessing memory.

-- We now present two patterns for lookup.
-- In the first case, we start with a field of bits
-- and look up based on that,
-- without making any assumptions about the meaning of the
-- bits.
-- In some functions, the bit keys are constructed
-- by concatenating bits containing
-- heterogeneous information, \textit{e.g.}
-- in difficult cases where polynomial approximation
-- works in one part of the domain but not in another.

-- In the second case, log-linear intervals,
-- we have a pattern integrating
-- key construction and lookup,
-- because correctly constructing keys to match log-linear
-- intervals is error-prone.

{- Register Lookup in $8$-Word Tables -}

-- TODO:  xxpermx could do 16-way or 32-way lookup together with xxlor

-- In this section we look at patterns for lookups
-- in tables of $8$ words.
-- In most applications,
-- the words are $32$-bit floating-point polynomial coefficients,
-- but not always; for example we also have one application
-- where the lookup retrieves bit masks.
-- Therefore we strive to keep the exposed components
-- of our tool set for this kind of pattern
-- as general and modular as possible,
-- to allow the domain expert to replace components of our high-level patterns,
-- or modify components when inter-component optimizations are possible.

-- For 8-way lookup,
-- the pattern has two main parts:
-- (1) constructing keys (maps of byte indices) and instructions
-- to perform the lookup, and
-- (2) constructing the lookup tables,
-- which are lists of register constants packed with
-- single-precision floating point values.

-- In the current pattern,
-- a table contains eight words,
-- and a single lookup selects one of these.
-- Since on POWER, the main instruction supporting such lookups, |vperm|,
-- does selection of Bytes, not words,
-- our lookup needs to locate four out of 32 bytes
-- to assemble one word,
-- and a single |xxperm| instruction allows exactly selection out of the 32 bytes contained in its first two register arguments.

-- Therefore we need 5 bits for lookup keys for the the individual byte,
-- and three of these, ``\texttt{kkk}'' will be the high-level key
-- for selection of one of the eight original alternative words.

-- Since application may produce these three-bit word-keys in many different ways,
-- we must not make any assumptions about their original alignment.
-- If the original alignment of the three key bits
-- falls within the five bits used by |xxperm|,
-- i.e., in one of the three byte patterns
-- \texttt{***kkk**}, \texttt{****kkk*}, and \texttt{*****kkk},
-- then we can directly use that key alignment;
-- otherwise we have to rotate the word-key into one of these three positions,
-- and we choose the last.

-- For the byte lookups,
-- the word-key ``\texttt{kkk}'' needs to be replicated four times,
-- and for each of the three possible word-key positions,
-- it is completed in a different way to four different five-bit byte-keys
-- (\ie, keys for byte selection),
-- resulting in |xxperm| argument vectors of the following shape:

-- \begin{center}
-- \begin{tabular}{cc}
-- \texttt{***kkk00 ***kkk01 ***kkk10 ***kkk11 } & or\cr
-- \texttt{***0kkk0 ***0kkk1 ***1kkk0 ***1kkk1 } & or\cr
-- \texttt{***00kkk ***01kkk ***10kkk ***11kkk }. \cr
-- \end{tabular}
-- \end{center}

-- The function |key8Word| generates these |xxperm| maps from
-- keys in different bit positions.
-- The bit positions are given as $\log_2$ of the positional value,
-- \ie, little-endian bit number,
-- with the right-most bit of the 32-bit word being considered as at position 0.

-- In detail, one alternative does rotation
-- within the word, |roti|, while the
-- other does it within the quadword, |rotqbii|.
-- The second instruction only supports rotations of
-- less than eight bits, so the relevant bits may
-- leave the component word in the second case.
-- As a result, the modulo arithmetic has to be
-- done differently in the two cases.

-- Since we can only join register values which
-- will become nodes in the code graph,
-- and not arbitrary Haskell data types,
-- we have to encapsulate the variation within
-- an auxiliary function (|splat|).

-- In the final step,
-- the ``select bytes'' instruction |xxsel|
-- uses the constant |mask| for selecting the key bits \texttt{kkk}
-- and the constant |c0123| for inserting the
-- \texttt{00}, \texttt{01},  \texttt{10},  \texttt{11} around the key
-- to generate a 5-bit lookup index
-- to be used by the |xxperm| in |lookup8Word| below
-- for look-up into 32-byte tables
-- (in the two registers addressable by a single |xxperm|).



-- key8Word :: forall repr. (repr VR) --idk what the right type is??
key8Word :: forall repr. (PowerISA repr) => Word8 -> repr VR -> repr VR
key8Word low v = xxsel c0123 look2 mask
  where
    -- To provide a simpler interface to the domain expert,
    -- we use the bit position of the $3$-bit key
    -- (passed in as the exponent of the place value of the |low| bit)
    -- to determine the minimum number of instructions to generate
    -- the key.
    -- If the key bits are within the five low-order bits of any byte,
    -- we do not need a rotate.
    -- In all cases we need to know the byte position of
    -- the bits (after rotation) so that byte can be replicated
    -- to all four bytes corresponding to each word being looked up.
    byte, bt :: Word8
    ((byte, bt), look2) = if lowBit `elem` ([0,1,2] :: [Word8])
      then  (low8, splt 16 v)     -- no rotation needed
      else  ( (1 + lowByte, 0)
            , splt 16 $ vrlq v (unbytes ([0,0,0,0,0,0,0,distance,0,0,0,0,0,0,0,0])) -- splat 4 $ vrlw v (unbytes [0,0,0,distance,0,0,0,0])
                -- TODO: clean
                -- Removed join and arbitrarily picked one as both are equivalent
                --equivJoin "to ways favour different units" "unknown"
                --[ splat 4 $ roti v distance
                --, splat 16 $ rotqbii v distance]
            )
    -- These masks depend on the (rotated) |bit| index of the lowest bit
    -- of the \texttt{kkk} key as it is located within the five-bit keys;
    -- the basis for this is the position |low|
    -- of the key in the function argument |v|,
    -- for which we calculate the byte-coordinates,
    -- and the number of bits we would have to rotate left
    -- to get |low| aligned on the lowest bit of a byte:
    low8@(lowByte, lowBit) = ((low :: Word8) `divMod` (8 :: Word8))
    distance = ((8 - lowBit) `mod` 8) :: Word8
    -- The auxiliary function |splat| has as its main task
    -- to produce a |xxperm| instruction that achieves
    -- replication of the three key bits ``\texttt{kkk}''
    -- over all four bytes of the respective word;
    -- the replication indices are calculated taking into account
    -- the possibility that |rotqbii| might have shifted the key
    -- over a word boundary, and around the quadword boundary (16 bytes).
    -- For |roti|, the |xxperm|-index needs to point within the same word,
    -- so |rotWidth| is instantiated to 4 in that case.
    -- (The generated indices all refer to the first argument of the |vperm| instruction, so the second argument is arbitrary.)

    -- splat :: forall repr. (PowerISA repr) => Word8 -> repr VR -> repr VR
    splt rotWidth x  = shufB1 x $ map (`mod` (16 :: Word8))
                      $ map (((3 - byte) `mod` rotWidth) +)
                      $ replicate 4 =<< ([0,4,8,12] :: [Word8])

  --   From the bit position, we look up the correct |mask|
  -- to use to insert the appropriate additional bits |c0123|.
    c0123, mask :: forall r. (PowerISA r) => r VR
    c0123 = case bt of
      0 -> unwrds4 0x00081018
      1 -> unwrds4 0x00011011
      2 -> unwrds4 0x00010203
      _ -> error "key8Word: impossible"
    mask = case bt of
      0 -> unwrds4 0x07070707
      1 -> unwrds4 0x0e0e0e0e
      2 -> unwrds4 0x1c1c1c1c
      _ -> error "key8Word: impossible"


-- Corresponding to different positions for the index bits, 
-- \texttt{kkk}, are three different ways of arranging the
-- bytes in the $16$-byte constants.
-- We zip the bytes with the desired order, sort by that and unzip, to reorder.

-- mk8WordTbl :: (SPUType a) => Integer -> [[Double]] -> [(VR a,VR a)]
mk8WordTbl :: PowerISA repr => Word8 -> [[Double]] -> [(repr VR, repr VR)]
mk8WordTbl low xs = map (mk8WordPair low) xs


-- mk8WordPair :: (SPUType a) => Integer -> [Double] -> (VR a,VR a)
mk8WordPair :: forall repr. (PowerISA repr) => Word8 -> [Double] -> (repr VR, repr VR)
mk8WordPair low xs =
    unbytes `prod` unbytes $ splitAt 16 permutedBytes
  where
    (x1s, x2s)     = splitAt 4 xs
    inOrderBytes    = bytes @Interp . unfloats =<< [x1s, x2s] --changed idSim

    permutedBytes  = map snd $ List.sort $ zip lexOrd inOrderBytes

    lexOrd = case (low `mod` 8) of
        1  -> [((i :: Integer),(j :: Integer),(k :: Integer))  | j <- [0..7], i <- [0..1], k <- [0..1]]
        2  -> [(j,i,0)  | j <- [0..7], i <- [0..3]]
        _  -> [(i,j,0)  | j <- [0..7], i <- [0..3]]


-- To insure that the lookup key is compatible with the format of the lookup table,
-- we encapsulate both parts in one pattern:

-- lookup8Word :: (SPUType a, HasJoin String (VR a)) => (Integer, Integer) -> [[Double]] -> VR a -> [VR a]
lookup8Word :: PowerISA repr =>
                     (Word8, Word8) -> [[Double]] -> repr VR -> [repr VR]
lookup8Word (high, low) tbl v = fst $ lookup8WordDev (high, low) tbl v
lookup8WordDev :: PowerISA repr =>
                        (Word8, Word8)
                        -> [[Double]] -> repr VR -> ([repr VR], [(String, repr VR)])
lookup8WordDev (high, low) tbl v =
    (result, [
      ("index", key8Word low v)
    ] ++ zipWith (\va (indx :: Integer) -> ("result" ++ show indx, va)) result [0..]
      ++ zipWith (\(va,_) (indx :: Int) -> ("regTbl" ++ show indx, va)) regTbl [0..]
      ++ zipWith (\(_,va) (indx :: Int) -> ("regTbl" ++ show indx, va)) regTbl [0..]
    )
  where
    regTbl = mk8WordTbl low tbl
    index (v1,v2) = vperm v1 v2 $ key8Word low v
    result = bitWidthErr "lookup8Word" high low $ map index regTbl

lookup8Word' :: PowerISA repr => repr VR -> (repr VR, repr VR) -> repr VR
lookup8Word' v (v1,v2) = vperm v1 v2 v
lookup8Words' :: PowerISA repr => repr VR -> [(repr VR, repr VR)] -> [repr VR]
lookup8Words' v = map (lookup8Word' v)



-- This pattern is typical in that a small number of
-- instructions are generated following a lot of
-- case checking to test different preconditions for
-- different combinations of instructions.
-- Many possible illegal parameters are filtered out,
-- and we try to return meaningful errors to the
-- domain expert rather than letting the
-- Haskell run-time system trap an irrefutable pattern.

-- The following error checking happens too frequently:

bitWidthErr :: (Num a1, Ord a1) => [Char] -> a1 -> a1 -> a2 -> a2
bitWidthErr name' high low = case concat
   [if high == low + 2 then "" else name' ++ ": need 3 bits"
   ,if high > 31 || low < 0  then name' ++ ": bounds"
                             else ""
   ] of
  [] -> id
  err -> error err

-- To debug 8-way lookup we use
testCoeffs :: (Num a, Enum a) => [[a]]
testCoeffs = [[1..8]]

{- Lookup in $16$-Word Table --- Lazy Higher-Order Code Generation -}

-- Similar to the $8$-way lookup in \sectref{lookup8Word},
-- we now provide functions to allocate
-- lists of floating-point numbers into registers for $16$-way lookups.
-- The function |mk16WordTbl| maps this over a list of such lists;
-- this is used when looking up coefficients for a list of $16$ polynomial segments.

-- The lookup of $16$ values requires $3$ |xxperm|s
-- to look up four words or $4$ |xxperm|s to look up $8$:
-- two to look up the high- and low-order halfwords,
-- and two to separate the first and second sets of four halfwords,
-- and interleave the high- and low-order parts.

-- mk16WordTbl :: (SPUType a) => [[Double]] -> [((VR a,VR a),(VR a,VR a))]
mk16WordTbl :: PowerISA repr => [[Double]] -> [((repr VR, repr VR), (repr VR, repr VR))]
mk16WordTbl = map mk16WordPair

-- mk16WordPair :: SPUType a => [Double] -> ((VR a,VR a),(VR a,VR a))
mk16WordPair :: PowerISA repr => [Double] -> ((repr VR, repr VR), (repr VR, repr VR))
mk16WordPair xs = if length xs == 16 then ((high0, high1), (low0, low1))
                                      else error "MathUtils.mk16WordPair"
  where
    -- Convert Haskell double-precision coefficients into register values
    -- and divide them into lists of high- and low-order halfwords.
    (high, low) = unInterleave $ concat $ map (shorts @Interp . unfloats) xsIn4s
    xsIn4s = in4s xs
    -- where zero is a constant needed here to force compile time evaluation of the
    -- lookup table, and \texttt{in4s} puts the list of doubles into lists of four.
    -- Now put the high and low shorts together
    highS = take 8 high ++ repeat 0  -- Pad with 0s if less than 8 elements
    lowS = take 8 low ++ repeat 0   -- Pad with 0s if less than 8 elements
    [high0, high1, low0, low1] = map unshorts [highS, lowS]





-- Split list into list of parts of size four.
in4s :: [a] -> [[a]]
in4s (a1:a2:a3:a4:as) = [a1,a2,a3,a4] : (in4s as)
in4s [] = []
in4s _ = error "in4s doesn't go evenly"

-- We look up values in such tables from single keys with the bit patterns
-- \texttt{000kkkk0 000kkkk1 }
-- where \texttt{kkkk} is the $16$-way lookup bit pattern,
-- and this halfword pattern is repeated for $16$ keys.
-- This involves four instructions

-- lookup16X2 :: SPUType a
--   => [((VR a,VR a),(VR a,VR a))] -> VR a -> ([VR a],[VR a])
lookup16X2 :: PowerISA repr => [((repr VR, repr VR), (repr VR, repr VR))] -> repr VR -> ([repr VR], [repr VR])
lookup16X2 tbl key = unzip $ map (flip l16X2l key) tbl

-- l16X2l :: SPUType a => ((VR a,VR a),(VR a,VR a)) -> VR a -> (VR a,VR a)
l16X2l :: PowerISA repr => ((repr VR, repr VR), (repr VR, repr VR)) -> repr VR -> (repr VR, repr VR)
l16X2l ((h0,h1),(l0,l1)) key = (v1,v2)
  where
    [v1,v2] = [vperm high low fstInterleave
              ,vperm high low sndInterleave
              ]
    high = vperm h0 h1 key
    low  = vperm l0 l1 key
    fstInterleave = unbytes  [ 0,   1,  16,  17,   2,   3,  18,  19
                             , 4,   5,  20,  21,   6,   7,  22,  23
                             ]
    sndInterleave = unbytes  [ 8,   9,  24,  25,  10,  11,  26,  27
                             ,12,  13,  28,  29,  14,  15,  30,  31
                             ]

-- Keys can be constructed in different ways depending on the application,
-- but some constructions would be hard to get right without language support.

{- Construct tables of |Double| values in memory -}
-- We are not using this version, because I forgot it was there.
-- The version we are using hides the |MemRegion| inside the loads and stores.
-- If there are multiple uses of the same table, the |HGM| machinery will
-- have to detect the sharing.
-- The lookups we are using do not handle the generation of breakpoints for
-- Maple or the generation of keys from double values.
-- They assume the key is already constructed as a bitfield.
-- This extra functionality should be factored out of the float versions
-- and wrapped around the pairwise lookup above.
-- The double lookups we are using do not need separate versions for different sizes.
-- }

-- The lookup of $8$ values requires $4$ \texttt{xxperm}s,
-- two to look up the high- and low-order halfwords,
-- and two to separate the first and second sets of four halfwords,
-- and interleave the high- and low-order parts.

-- Therefore there is no way a call site can communicate to |dblMemTables|
-- which |VR a| type to use for determining an instance.

-- This is resolved by having a functional dependency
-- in the class declaration of |MemRegion|
-- that determines |VR a|, given a type for |cm|.

-- Used for double precision but not needed right now 
-- dblMemTables :: forall a n . (SPUType a, NatE n) => n -> [[Double]] -> [LSMR a]
-- dblMemTables n xs = map dblMemPair xs
--   where
--     -- dblMemPair :: [Double] -> LSMR a
--     dblMemPair ds = initLSMR "dblMemTables" n (dMP' ds)

--     --dMP' :: [Double] -> [c]
--     dMP' (d1:d2:ds) = (undoubles [d1,d2]) : (dMP' ds)
--     dMP' [d1] = [undoubles [d1, 0]]
--     dMP' [] = []

-- Looking up double-word values in tables as constructed above from single keys with the bit patterns
-- \texttt{00...0kk...kxxxx xx...x xx...x xx...x }
-- where \texttt{kk.k} is the $2^n$-way lookup bit pattern,
-- involves four instructions

-- Used for double precision but not needed right now 
-- dblMemLookup :: SPUType a => [LSMR a] -> (VR a,VR a) -> ([VR a],[VR a])
-- dblMemLookup tbls key = unzip $ map (dblML key) tbls

-- Look up two double-word values for each of two bit strings in
-- the first word slot.
-- Assume that the keys have the higher bits masked out.


-- Used for double precision but not needed right now 
-- dblML :: SPUType a => (VR a,VR a) -> LSMR a -> (VR a,VR a)
-- dblML (key1,key2) tbl =  (xxperm high low fstInterleave
--                          ,xxperm high low sndInterleave
--                          )
--   where
--     high   = fst $ lqxMR key1 tbl
--     low    = fst $ lqxMR key2 tbl
--     -- and requires two auxiliary lookup constants:
--     fstInterleave = unbytes  $ [0..7] ++ [16..23]
--     sndInterleave = unbytes  $ [8..15] ++ [24..31]

-- Keys can be constructed in different ways depending on the application,
-- but some constructions would be hard to get right without language support,
-- as we see in the next section.

-- Eventually we should:
-- - make this work for 4-way, 8-way, 16-way, 32-way
-- - straddling boundary means that you can't use |xvmaddmsp| to get to the right place in one step

-- We use the following data structure for specifying lookup schemes:

data RegLookupSpec = RegLookupSpec
  {mantissaBits    :: Int
  ,exponentBits    :: Int
  ,skipIntervals   :: Int
  ,rangeEnd        :: Double
  }
  deriving (Show)

lookupBits :: RegLookupSpec -> Int
lookupBits spec = mantissaBits spec + exponentBits spec

-- lookupBreaks :: RegLookupSpec -> [Double]
lookupBreaks :: RegLookupSpec -> [Double]
lookupBreaks spec =
    if skip_ >= 2 ^ (mant_+1) || skip_ < 0 || mant_ < 0 || exp_ < 0
    then error $ "lookupBreaks parameter problem" ++ show (skip_, mant_, exp_)
    else map (/ scale) b
  where
    skip_ = skipIntervals spec
    exp_  = exponentBits spec
    mant_ = mantissaBits spec
    b :: [Double]
    b =  scanl (+) 0 $ take (2^(mant_ + exp_)) $ drop skip_ $
         concat [replicate (2^mant_) $ 2**(fromIntegral i) | (i :: Integer) <- [ -3 .. ]]
    -- To save a conversion at run-time, we allow the user to specify, via |endpoint|,
    -- the total width of all the intervals (which start at 0),
    -- which is used to scale the break points between intervals.
    scale = last b / rangeEnd spec


-- For functions evaluated over extended ranges,
-- it is necessary to evaluate a polynomial in $x-\bar{x}$
-- to prevent excessive truncation errors.
evenSPBreaks :: RegLookupSpec -> [Double]
evenSPBreaks spec = map roundToSP $ lookupBreaks spec

roundToSP :: Double -> Double
roundToSP x = head $ floats @Interp $ unfloats4 x

-- The extended record |LookupSpec| just memoises those two functions:

-- lookupMemo :: RegLookupSpec -> LookupSpec
lookupMemo :: RegLookupSpec -> LookupSpec
lookupMemo spec = LookupSpec
  { mant       = mantissaBits   spec
  , expo       = exponentBits   spec
  , skip       = skipIntervals  spec
  , totalBits  = lookupBits     spec
  , endPoint   = rangeEnd       spec
  , breaks     = lookupBreaks   spec
  }

-- Eventually we should:
-- - make this work for 4-way, 8-way, 16-way, 32-way
-- - straddling boundary means that you can't use |xvmaddmsp| to get to the right place in one step

-- Data structure for specifying lookup schemes,
-- not safe to construct without help of helper functions:

data LookupSpec = LookupSpec {mant       :: Int
                             ,expo       :: Int
                             ,skip       :: Int
                             ,totalBits  :: Int
                             ,endPoint   :: Double
                             ,breaks     :: [Double]
                             }


-- |LookupSpec| values are expected to satisfy the invariant
-- forall spec :: LookupSpec . totalBits spec = mant spec + expo spec

-- To simplify lookups for the user,
-- we encapsulate the defining properties of the
-- particular lookup in a data structure |LookupSpec|,
-- which contains the numbers of bits from the mantissa and the exponent
-- to be used in the lookup,
-- thereby defining the relative sizes of the intervals.
-- For example, with one exponent bit and three mantissa bits,
-- eight intervals of size $a$ will be followed by eight of size $2a$.
-- For added flexibility,
-- we allow the user to specify a number of the first interval size to skip,
-- so in the above,
-- skipping one interval would result in seven size $a$,
-- eight size $2a$ and one size $4a$ intervals.

calcBreaks :: Int -> Int -> Int -> Double -> LookupSpec
calcBreaks mant' exp' skip' endPoint' = lookupMemo $ RegLookupSpec
  {mantissaBits    = mant'
  ,exponentBits    = exp'
  ,skipIntervals   = skip'
  ,rangeEnd        = endPoint'
  }

-- The calculated break points are used both to construct the approximations
-- (using Maple in our case), and to generate the code to construct
-- the lookup key at run-time,
-- in a function that accepts,
-- besides the lookup specification and the list of coefficient lists,
-- a pair of two arguments for which the lookup is performed in parallel,
-- returning a pair of retrieved lists of coefficients:

-- lookup16X2Coeffs :: (SPUType a)
--   => LookupSpec -> [[Double]] -> (VR a,VR a) -> ([VR a],[VR a])

-- We do not go into further detail concerning the construction
-- of these lookup tables and of the machine instructions for performing the lookup,
-- since these are similar in spirit to those of \sectref{lookup8Word}.
-- Instead, we explain in more detail
-- a non-trivial application
-- of characteristics of the host programming language Haskell.

-- lookup16X2Coeffs :: (HasJoin String (VR a), SPUType a) => LookupSpec -> [[Double]] -> (VR a,VR a) -> ([VR a],[VR a])
lookup16X2Coeffs :: PowerISA repr => LookupSpec -> [[Double]] -> (repr VR, repr VR) -> ([repr VR], [repr VR])
lookup16X2Coeffs spec rawCoeffs (v1,v2)
    = if totalBits spec /= 4
        then error "lookup16X2Coeffs only implemented for 16-way lookup"
        else lookup16X2 (mk16WordTbl $ rotLists rawCoeffs) lookup'
  where
  --   to get the exponent bits to end with $4$ zero bits for the beginning of
  -- the first interval, we position the beginning of the interval at $2$.
  -- The first $2^{\mant}-\leap$ break points
  -- are in the first exponent level $[2,4)$, so we need to start at
  -- $2 + \leap 2^{1-\mant}$.
  -- The first sup-interval divides into $[2,4]$ $2^{\mant}$ times,
  -- so the scale is calculable as $2^{1-\mant}/(b_1-b_0)$\restorecolumns
    offset = unfloats4 $ 2 + (fromIntegral $ skip spec) * 2**(fromIntegral $ 1-(mant spec))
    scale  = unfloats4 $ 2**(1 - (fromIntegral $ mant spec))
                         / (((breaks spec) !! 1) - ((breaks spec) !! 0))
  -- the bits we want for the lookup are $(8-\expo)...(9+\mant)$,
  -- which crosses a byte boundary if $\mant \ne 0$,
  -- so we need to use a rotate\restorecolumns
    rightBits v = reven {-equivJoin "odd or even unit" "unknown" [reven,rodd]-}
      where
        -- rodd      = vrlq vso (unbytes [0,0,0,0,0,0,0,bitShift])
        reven     = vrlw vso (unbytes16 bitShift)
        vso       = xvmaddmsp v scale offset
        bitShift  = fromIntegral $ mant spec + 2
    -- put the bits from word $0$ in bytes $0$ and $1$, etc., this is why we need to unroll\restorecolumns
    shuffled = vperm  (rightBits v1)
                      (rightBits v2)
                      (unbytes [0,0, 4,4, 8,8, 12,12, 16,16, 20,20, 24,24, 28,28])
  --   which is where we would waste an instruction by doing only one lookup in parallel.
  -- Now merge these bits to get lookups from two positive inputs\restorecolumns
    lookup' = xxsel (unshorts8 0x0001) shuffled (unbytes16 $ 32 - 2)
    -- To provide a nicer interface, we generate the register constants for the
    -- tables here.
    rotLists = map (swapAt (2 ^ (totalBits spec) - skip spec))

-- |lookupQuadsX2Coeffs| has two suspicious unused arguments,
-- and two unused local variables |lookup| and |rotLists|!
-- It is currently unused --- should be removed?

-- lookupQuadsX2Coeffs :: (HasJoin String (VR a), SPUType a, NatE n) => RegLookupSpec -> [[[Integer]]] -> n -> (VR a,VR a) -> (VR a,VR a)
lookupQuadsX2Coeffs :: PowerISA repr => RegLookupSpec -> p1 -> p2 -> (repr VR, repr VR) -> (repr VR, repr VR)
lookupQuadsX2Coeffs spec' _rawCoeffs _size (v1,v2)
    = (rightBits v1, rightBits v2)
  where
    spec = lookupMemo spec'
    -- to get the exponent bits to end with $4$ zero bits for the beginning of
    -- the first interval, we position the beginning of the interval at $2$.
    -- The first $2^{\mant}-\leap$ break points
    -- are in the first exponent level $[2,4)$, so we need to start at
    -- $2 + \leap 2^{1-\mant}$.
    -- The first sup-interval divides into $[2,4]$ $2^{\mant}$ times,
    -- so the scale is calculable as $2^{1-\mant}/(b_1-b_0)$
    offset = undoubles2 $ 2 + (fromIntegral $ skip spec) * 2**(fromIntegral $ 1-(mant spec))
    scale  = undoubles2 $ 2**(1 - (fromIntegral $ mant spec))
                          / (((breaks spec) !! 1) - ((breaks spec) !! 0))
    -- the bits we want for the lookup are $(8-\expo)...(9+\mant)$,
    -- which crosses a byte boundary if $\mant \ne 0$,
    -- so we need to use a rotate
    rightBits v = reven {-equivJoin "odd or even unit" "unknown" [reven,rodd]-}
      where
        -- rodd      = vrlq vso (unbytes[0,0,0,0,0,0,0,bitShift])
        reven     = vrlw vso (unbytes16 bitShift)  
        vso       = xvmaddmdp v scale offset
        bitShift  = fromIntegral $ mant spec + 2
        -- put the bits from word $0$ in bytes $0$ and $1$, etc., this is why we need to unroll
    shuffled = vperm  (rightBits v1)
                      (rightBits v2)
                      (unbytes [0,0, 4,4, 8,8, 12,12, 16,16, 20,20, 24,24, 28,28])
    -- which is where we would waste an instruction by doing only one lookup in parallel.
    -- Now merge these bits to get lookups from two positive inputs
    _lookup = xxsel (unshorts8 0x0001) shuffled (unbytes16 $ 32 - 2)
    -- To provide a nicer interface, we generate the register constants for the
    -- tables here.
    _rotLists = map (swapAt (2 ^ (totalBits spec) - skip spec))


-- Since |lookup16X2Coeffs| allows parallel lookup of two keys,
-- we offer a wrapper function (which we used in \sectref{tanh})
-- that ``unrolls'' an argument function twice to make use of this.

-- use16X2lookup :: (SPUType a)
--   => RegLookupSpec -> [[Double]]
--   -> ([VR a] -> arg -> (VR a, result))
--   -> (arg, arg) -> (result, result)

-- I left the type principal for the time being.

-- use16X2lookup :: (HasJoin String (VR a), SPUType a)
--    => RegLookupSpec
--    -> [[Double]]
--    -> ([VR a] -> arg -> (VR a, result))
--    -> (arg, arg) -> (result, result)

-- The implementation of this function
-- ``ties the knot'' by creating the kind of apparently recursive data
-- dependencies that can only be resolved in a non-strict programming
-- language like Haskell,
-- when an appropriately non-strict argument function |mkKeyResult|
-- is supplied for which the |key| result does not depend on the |coeffs|
-- argument.

-- use16X2lookup :: (repr VR) => RegLookupSpec -> [[Double]] -> ([repr VR] -> arg -> (repr VR, result)) -> (arg, arg) -> (result, result)
use16X2lookup :: PowerISA repr =>
                       RegLookupSpec
                       -> [[Double]]
                       -> ([repr VR] -> t -> (repr VR, b))
                       -> (t, t)
                       -> (b, b)
use16X2lookup spec rawCoeffs mkKeyResult (v1, v2) = let
    (coeffs1, coeffs2) = lookup16X2Coeffs  (lookupMemo spec)
                                           rawCoeffs (key1, key2)
    (key1, result1)    = mkKeyResult coeffs1 v1
    (key2, result2)    = mkKeyResult coeffs2 v2
  in (result1, result2)



  -- use16X2lookupDev :: (PowerISA repr) => RegLookupSpec -> [[Double]] -> ([repr VR] -> arg -> ((repr VR, result),[(String,repr VR)])) -> (arg, arg) -> ((result,result), [(String,repr VR)])
use16X2lookupDev :: PowerISA repr =>
                          RegLookupSpec
                          -> [[Double]]
                          -> ([repr VR] -> t -> ((repr VR, b), [(String, repr VR)]))
                          -> (t, t)
                          -> ((b, b), [(String, repr VR)])
use16X2lookupDev spec rawCoeffs mkKeyResult (v1, v2) = let
    (coeffs1, coeffs2) = lookup16X2Coeffs  (lookupMemo spec)
                                           rawCoeffs (key1, key2)
    ((key1, result1),dbg1)    = mkKeyResult coeffs1 v1
    ((key2, result2),dbg2)    = mkKeyResult coeffs2 v2
  in ((result1, result2)
     , dbg1 ++ dbg2 ++ [("key1",key1),("key2",key2)]
     )

-- In \figref{use16X2lookup1},
-- we show a direct code graph representation of this function:
-- For each of the two calls to |mkKeyResult|,
-- which are drawn with arrow from argument nodes to the |mkKeyResult| hyperedge,
-- and arrows from the hyperedge to its results,
-- there appears to be a cycle around |key| and |coeffs|.

-- We can make it explicit that |key| does not depend on |coeffs|,
-- for example by specialising for |mkKeyResult| functions of the following shape:

-- mkKeyResult coeffs v = let w = pre v
--     in (mkKey w, mkResult v w coeffs)

-- This definition is depicted in \figref{mkKeyResult2};
-- since there is no path in this from the |coeffs| input to the |key| output,
-- application of |use16X2lookup| to a function of this shape,
-- depicted in \figref{use16X2lookup2},
-- is cycle-free, and therefore can be used for code generation.
-- Since our |tanhSPU| function from \sectref{tanh}
-- is built using |use16X2lookup1|,
-- it is no pure coincidence
-- that the resulting code graph in \figref{tanhCG}
-- exhibits the same symmetries as \figref{use16X2lookup2}.

-- Although this pattern may at first sight appear to be somewhat convoluted,
-- it has the advantage that is guarantees
-- that the argument function |mkKeyResult| can use only one lookup;
-- the two lookups required by the two calls to |mkKeyResult|
-- are then parallelised in |lookup16X2Coeffs|.
-- The ``obvious'' reformulation
-- with a second-order function of type
-- |((VR a -> [VR a]) -> arg -> result)|
-- as third argument fails to establish this guarantee,
-- and therefore would not enable this parallelisation.

{- Memory-Based Lookup -}
-- If we want to lookup doubles, from large tables, it makes sense to
-- look up two coefficients at a time from memory, and then use
-- |vperm| to put them in the right order.

-- The calculated break points are then used both to construct the approximations
-- (using Maple in our case), and to generate the code to construct
-- the lookup key at run-time:

-- Used for double precision but not needed right now 
-- coeffsMem8Tbl :: forall a n . (NatE n, HasJoin String (VR a), SPUType a) =>
--   LookupSpec -> n -> [[Double]] -> [LSMR a]
-- coeffsMem8Tbl _spec n rawCoeffs = dblMemTables n rawCoeffs

-- Used for double precision but not needed right now 
-- coeffsMem8 :: forall a . (HasJoin String (VR a), SPUType a) =>
--   LookupSpec -> [LSMR a] -> VR a -> ([VR a],[VR a])
-- coeffsMem8 spec tbls v =  dblMemLookup tbls (key0,key1)
--   where
--     -- |dmt :: [[Double]] -> [cm]|
--     -- |dmt = dblMemTables|
--     -- |tbls :: [cm]|
--     -- |tbls = dmt rawCoeffs|

--     -- to get the exponent bits to end with $4$ zero bits for the beginning of
--     -- the first interval, we position the beginning of the interval at $2$.
--     -- The first $2^{\mant}-\leap$ break points
--     -- are in the first exponent level $[2,4)$, so we need to start at
--     -- $2 + \leap 2^{1-\mant}$.
--     -- The first sup-interval divides into $[2,4]$ $2^{\mant}$ times,
--     -- so the scale is calculable as $2^{1-\mant}/(b_1-b_0)$
--     offsetD :: Double
--     offsetD = 2 + (fromIntegral $ skip spec) * 2**(fromIntegral $ 1 - mant spec)
--     scaleD :: Double
--     scaleD  = 2 ** (1 - (fromIntegral $ mant spec))
--                          / ((breaks spec !! 1) - (breaks spec !! 0))

--     offsetV, scaleV :: repr VR
--     offsetV  = unfloats4 offsetD
--     scaleV   = unfloats4 scaleD

--     -- the bits we want for the lookup are $(13-\expo)...(12+\mant)$.
--     -- We need to rotate the last bit into bit $28$ using |roti|.

--     -- The key is generated using an affine transformation.
--     -- If the offset is zero or the slope one, then simpler
--     -- forms using fewer operands can be substituted to eliminate
--     -- register constants.
--     -- These transformations are in the hands of the Domain expert,
--     -- so these values can be arranged if they happen to fall
--     -- within the range of acceptable values.

--     key0 = vrlw vso (unbytes [0,0,0,bitShift,0,0,0,0])
--       where
--         vso = if scaleD == 1
--                  then if offsetD == 0
--                    then v
--                    else xvadddp v offsetV
--                  else if offsetD == 0
--                    then xvmuldp v scaleV
--                    else xvmaddmdp v scaleV offsetV
--         bitShift :: Word8
--         bitShift = fromIntegral $ (mant spec + 12 - 28) `div` 32

--   -- The second key is in the right bit position in the second double-word.
--     key1 = vrlq key0 (unbytes[0,0,0,0,0,0,0,8*8]) -- multiply by 8 because rotqbyi was a byte rotate and vrlq is a bit rotate 

-- type Dbg a = [(String,repr VR)]



-------- TODO ---------

splat :: (PowerISA repr) => Word8 -> Word8 -> repr VR -> repr VR
splat rotWidth byte x  = shufB1 x $ map (`mod` (16 :: Word8))
                  $ map (((3 - byte) `mod` rotWidth) +)
                  $ replicate 4 =<< ([0,4,8,12] :: [Word8])

shufB :: (PowerISA repr) => repr VR -> repr VR -> [Word8] -> repr VR
shufB x y = vperm x y . unbytes -- TODO: replace/move later 
shufB1 :: PowerISA repr => repr VR -> [Word8] -> repr VR
shufB1 x = shufB x x

cgtbi :: (PowerISA repr1, PowerISA repr2, Integral a) =>
               repr1 VR -> a -> repr2 VR
cgtbi ra s16 = cmpsi bytes unbytes (>) ra (sign (8 :: Word8) ((0xff :: Word8) .&. fromIntegral s16))
