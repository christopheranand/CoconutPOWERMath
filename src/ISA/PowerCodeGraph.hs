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
{-# LANGUAGE FlexibleContexts #-}


module ISA.PowerCodeGraph (module ISA.PowerCodeGraph, module Coconut.Core.CodeGraph) where

import Coconut.BaseTypes
import Coconut.Graph.CodeGraph
import Coconut.Utils.CGWrappers
import Data.Int

import ISA.PowerISA
import ISA.PowerInterp
import Coconut.Core.CoreISA
import Coconut.Core.CodeGraph

import Data.Word

instance (Hardware h,CoreISA (Graph h)) => PowerISA (Graph h) where

  unintegerSG :: Integer -> Graph h GPR
  unintegerSG i   = {-# SCC "ISA.PowerCodeGraph.unintegerSG"   #-}
      op_c0 @h (Instruction [fromIntegral i] "unintegerSG"
               ((unintegerSG @Interp) i))
               (runInterpGPR ((unintegerSG @Interp) i)
               ,runInterpGPR ((unintegerSG @Interp) i))



  unfloats4 :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => Double -> Graph h VR
  unfloats4 imm   = {-# SCC "Core.CodeGraph.unfloats4"     #-}
                  op_c0 @h (Instruction (ISA.PowerInterp.tupleWord64ToListInt $ runInterpVR $ (unfloats4 @Interp) imm) "unfloats4" ((unfloats4 @Interp) imm))
                            (runInterpVR ((unfloats4 @Interp) imm))

  lvx :: Graph h MR -> Graph h GPR -> (Graph h VR, Graph h MR)
  lvx m d = {-# SCC "ISA.PowerCodeGraph.lvx"   #-}
                    op_m2 @h (Instruction [] "lvx"
                              (\m0 d0 -> lvx @Interp m0 d0)) (0,1) m d

  stvx :: Graph h MR -> Graph h GPR -> Graph h VR -> Graph h MR
  stvx m d v = {-# SCC "ISA.PowerCodeGraph.stvx"   #-}
                    op_m3 @h (Instruction [] "stvx"
                              (\m0 d0 v0 -> stvx @Interp m0 d0 v0)) (0,0) m d v

  lxv :: Graph h MR -> Int -> (Graph h VR, Graph h MR)
  lxv m d = {-# SCC "ISA.PowerCodeGraph.lxv"   #-}
                    op_m1 @h (Instruction [d] "lxv"
                              (\m0 -> lxv @Interp m0)) (0,1) m

  stxv :: Graph h MR -> Int -> Graph h VR -> Graph h MR
  stxv m d v = {-# SCC "ISA.PowerCodeGraph.stxv"   #-}
                    op_m2 @h (Instruction [d] "stxv"
                              (\m0 v0 -> stxv @Interp m0 v0)) (0,0) m v
--

  absG :: Graph h GPR -> Graph h GPR
  absG x = {-# SCC "ISA.PowerCodeGraph.absG" #-}
    op1 @h (Instruction [] "absG" (absG @Interp)) x

  addcG :: Graph h GPR -> Graph h GPR -> Graph h GPR
  addcG x y = {-# SCC "ISA.PowerCodeGraph.addcG" #-}
    op2 @h (Instruction [] "addcG" (addcG @Interp)) x y

  addiG :: Graph h GPR -> Int -> Graph h GPR
  addiG x i = {-# SCC "ISA.PowerCodeGraph.addiG" #-}
    op1 @h (Instruction [fromIntegral i] "addiG"
            (\x0 -> addiG @Interp x0 i)) x

  subfG    :: Graph h GPR -> Graph h GPR -> Graph h GPR
  subfG x y = {-# SCC "ISA.PowerCodeGraph.subfG" #-}
    op2 @h (Instruction [] "subfG" (subfG @Interp)) x y

  subficG    :: Graph h GPR -> Int -> Graph h GPR
  subficG x i = {-# SCC "ISA.PowerCodeGraph.subficG" #-}
    op1 @h (Instruction [fromIntegral i] "subficG"
            (\x0 -> subficG @Interp x0 i)) x

  negG      :: Graph h GPR -> Graph h GPR
  negG x = {-# SCC "ISA.PowerCodeGraph.negG" #-}
    op1 @h (Instruction [] "negG" (negG @Interp)) x

  mulhdG   :: Graph h GPR -> Graph h GPR -> Graph h GPR
  mulhdG x y = {-# SCC "ISA.PowerCodeGraph.mulhdG" #-}
    op2 @h (Instruction [] "mulhdG" (mulhdG @Interp)) x y

  mulldG   :: Graph h GPR -> Graph h GPR -> Graph h GPR
  mulldG x y = {-# SCC "ISA.PowerCodeGraph.mulldG" #-}
    op2 @h (Instruction [] "mulldG" (mulldG @Interp)) x y

  mulhduG :: Graph h GPR -> Graph h GPR-> Graph h GPR
  mulhduG x y = {-# SCC "ISA.PowerCodeGraph.mulhduG" #-}
    op2 @h (Instruction [] "mulhduG" (mulhduG @Interp)) x y

  divdG    :: Graph h GPR -> Graph h GPR -> Graph h GPR
  divdG x y = {-# SCC "ISA.PowerCodeGraph.divdG" #-}
    op2 @h (Instruction [] "divdG" (divdG @Interp)) x y

  divduG   :: Graph h GPR -> Graph h GPR -> Graph h GPR
  divduG x y = {-# SCC "ISA.PowerCodeGraph.divduG" #-}
    op2 @h (Instruction [] "divduG" (divduG @Interp)) x y

  rldiclG :: Graph h GPR -> Integer -> Integer -> Graph h GPR
  rldiclG x i1 i2 = {-# SCC "ISA.PowerCodeGraph.rldiclG" #-}
    op1 @h (Instruction [fromIntegral i1, fromIntegral i2] "rldiclG"
            (\x0 -> rldiclG @Interp x0 i1 i2))
            x

  rldicrG :: Graph h GPR -> Integer -> Integer -> Graph h GPR
  rldicrG x i1 i2 = {-# SCC "ISA.PowerCodeGraph.rldicrG" #-}
    op1 @h (Instruction [fromIntegral i1, fromIntegral i2] "rldicrG"
            (\x0 -> rldicrG @Interp x0 i1 i2))
            x

  rldimiG :: Graph h GPR {- inOut -} -> Graph h GPR -> Integer -> Integer -> Graph h GPR
  rldimiG x y i1 i2 = {-# SCC "ISA.PowerCodeGraph.rldimiG" #-}
    op2 @h (Instruction [fromIntegral i1, fromIntegral i2] "rldimiG"
            (\x0 y0 -> rldimiG @Interp x0 y0 i1 i2))
            x y

  rldicG   :: Graph h GPR -> Integer -> Integer -> Graph h GPR
  rldicG x i1 i2 = {-# SCC "ISA.PowerCodeGraph.rldicG" #-}
    op1 @h (Instruction [fromIntegral i1, fromIntegral i2] "rldicG"
            (\x0 -> rldicG @Interp x0 i1 i2))
            x

  rldclG   :: Graph h GPR -> Graph h GPR -> Integer -> Graph h GPR
  rldclG x y i = {-# SCC "ISA.PowerCodeGraph.rldclG" #-}
    op2 @h (Instruction [fromIntegral i] "rldclG"
            (\x0 y0 -> rldclG @Interp x0 y0 i))
            x y

  rldcrG   :: Graph h GPR -> Graph h GPR -> Integer -> Graph h GPR
  rldcrG x y i = {-# SCC "ISA.PowerCodeGraph.rldcrG" #-}
    op2 @h (Instruction [fromIntegral i] "rldcrG"
            (\x0 y0 -> rldcrG @Interp x0 y0 i))
            x y

  rlwimiG :: Graph h GPR {- inOut -} -> Graph h GPR -> Integer -> Integer -> Integer -> Graph h GPR
  rlwimiG x y i1 i2 i3 = {-# SCC "ISA.PowerCodeGraph.rlwimiG" #-}
    op2 @h (Instruction [fromIntegral i1, fromIntegral i2, fromIntegral i3 ] "rlwimiG"
            (\x0 y0 -> rlwimiG @Interp x0 y0 i1 i2 i3))
            x y

  add :: Graph h GPR -> Graph h GPR -> Graph h GPR
  add x y = {-# SCC "ISA.PowerCodeGraph.add" #-}
    op2 @h (Instruction [] "add" (add @Interp)) x y

  xoriG :: Graph h GPR -> Word -> Graph h GPR
  xoriG x w = {-# SCC "ISA.PowerCodeGraph.xoriG" #-}
    op1 @h (Instruction [fromIntegral w] "xoriG"
      (\x0 -> xoriG @Interp x0 w)) x

  eqvG :: Graph h GPR -> Graph h GPR -> Graph h GPR
  eqvG x y = {-# SCC "ISA.PowerCodeGraph.eqvG" #-}
    op2 @h (Instruction [] "eqvG" (eqvG @Interp)) x y

  nandG :: Graph h GPR -> Graph h GPR -> Graph h GPR
  nandG x y = {-# SCC "ISA.PowerCodeGraph.nandG" #-}
    op2 @h (Instruction [] "nandG" (nandG @Interp)) x y

  -- orG :: Graph h GPR -> Graph h GPR -> Graph h GPR
  -- orG x y = {-# SCC "ISA.PowerCodeGraph.orG" #-}
  --   op2 @h (Instruction [] "orG" (orG @Interp)) x y

  norG :: Graph h GPR -> Graph h GPR -> Graph h GPR
  norG x y = {-# SCC "ISA.PowerCodeGraph.norG" #-}
    op2 @h (Instruction [] "norG" (norG @Interp)) x y

  cmp :: Graph h GPR -> Graph h GPR -> Graph h GPR -> Graph h GPR
  cmp x y z = {-# SCC "ISA.PowerCodeGraph.cmp" #-}
    op3 @h (Instruction [] "cmp" (cmp @Interp)) x y z

  divdu :: Graph h GPR -> Graph h GPR -> Graph h GPR
  divdu x y = {-# SCC "ISA.PowerCodeGraph.divdu" #-}
    op2 @h (Instruction [] "divdu" (divdu @Interp)) x y

  andc :: Graph h GPR -> Graph h GPR -> Graph h GPR
  andc x y = {-# SCC "ISA.PowerCodeGraph.andc" #-}
    op2 @h (Instruction [] "andc" (andc @Interp)) x y

  eqv   :: Graph h GPR -> Graph h GPR -> Graph h GPR
  eqv x y = {-# SCC "ISA.PowerCodeGraph.eqv" #-}
    op2 @h (Instruction [] "eqv" (eqv @Interp)) x y

  cntlzd :: Graph h GPR -> Graph h GPR
  cntlzd x = {-# SCC "ISA.PowerCodeGraph.cntlzd" #-}
    op1 @h (Instruction [] "cntlzd" (cntlzd @Interp)) x

  cntlzw :: Graph h GPR -> Graph h GPR
  cntlzw x = {-# SCC "ISA.PowerCodeGraph.cntlzw" #-}
    op1 @h (Instruction [] "cntlzw" (cntlzw @Interp)) x


{-

TODO - complete merging un.. functions

-}
  vsel :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  vsel x y z = {-# SCC "ISA.PowerCodeGraph.vsel" #-}
    op3 @h (Instruction [] "vsel" (vsel @Interp)) x y z

  xxsel :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xxsel x y z = {-# SCC "ISA.PowerCodeGraph.xxsel" #-}
    op3 @h (Instruction [] "xxsel" (xxsel @Interp)) x y z

  vperm :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  vperm x y z = {-# SCC "ISA.PowerCodeGraph.vperm" #-}
    op3 @h (Instruction [] "vperm" (vperm @Interp)) x y z

  vpermr :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  vpermr x y z = {-# SCC "ISA.PowerCodeGraph.vpermr" #-}
    op3 @h (Instruction [] "vpermr" (vpermr @Interp)) x y z

  vspltb :: Graph h VR -> Integer -> Graph h VR
  vspltb x i = {-# SCC "ISA.PowerCodeGraph.vspltb" #-}
      op1 @h (Instruction [fromIntegral i] "vspltb"
              (\x0 -> vspltb @Interp x0 i)) x

  vmrghb :: Graph h VR -> Graph h VR -> Graph h VR
  vmrghb x y = {-# SCC "ISA.PowerCodeGraph.vmrghb" #-}
    op2 @h (Instruction [] "vmrghb" (vmrghb @Interp)) x y

  vmrglb :: Graph h VR -> Graph h VR -> Graph h VR
  vmrglb x y = {-# SCC "ISA.PowerCodeGraph.vmrglb" #-}
    op2 @h (Instruction [] "vmrglb" (vmrglb @Interp)) x y

  vmrghh :: Graph h VR -> Graph h VR -> Graph h VR
  vmrghh x y = {-# SCC "ISA.PowerCodeGraph.vmrghh" #-}
    op2 @h (Instruction [] "vmrghh" (vmrghh @Interp)) x y

  vmrglh :: Graph h VR -> Graph h VR -> Graph h VR
  vmrglh x y = {-# SCC "ISA.PowerCodeGraph.vmrglh" #-}
    op2 @h (Instruction [] "vmrglh" (vmrglh @Interp)) x y

  vmrghw :: Graph h VR -> Graph h VR -> Graph h VR
  vmrghw x y = {-# SCC "ISA.PowerCodeGraph.vmrghw" #-}
    op2 @h (Instruction [] "vmrghw" (vmrghw @Interp)) x y

  vmrglw :: Graph h VR -> Graph h VR -> Graph h VR
  vmrglw x y = {-# SCC "ISA.PowerCodeGraph.vmrglw" #-}
    op2 @h (Instruction [] "vmrglw" (vmrglw @Interp)) x y

  vmrgew :: Graph h VR -> Graph h VR -> Graph h VR
  vmrgew x y = {-# SCC "ISA.PowerCodeGraph.vmrgew" #-}
    op2 @h (Instruction [] "vmrgew" (vmrgew @Interp)) x y


  vmrgow :: Graph h VR -> Graph h VR -> Graph h VR
  vmrgow x y = {-# SCC "ISA.PowerCodeGraph.vmrgow" #-}
    op2 @h (Instruction [] "vmrgow" (vmrgow @Interp)) x y


  vclzb :: Graph h VR -> Graph h VR
  vclzb x = {-# SCC "ISA.PowerCodeGraph.vclzb" #-}
    op1 @h (Instruction [] "vclzb" (vclzb @Interp)) x


  vclzh :: Graph h VR -> Graph h VR
  vclzh x = {-# SCC "ISA.PowerCodeGraph.vclzh" #-}
    op1 @h (Instruction [] "vclzh" (vclzh @Interp)) x

  vclzw :: Graph h VR -> Graph h VR
  vclzw x = {-# SCC "ISA.PowerCodeGraph.vclzw" #-}
    op1 @h (Instruction [] "vclzw" (vclzw @Interp)) x

  vclzd :: Graph h VR -> Graph h VR
  vclzd x = {-# SCC "ISA.PowerCodeGraph.vclzd" #-}
    op1 @h (Instruction [] "vclzd" (vclzd @Interp)) x

  vclzdm :: Graph h VR -> Graph h VR -> Graph h VR
  vclzdm x y = {-# SCC "ISA.PowerCodeGraph.vclzdm" #-}
    op2 @h (Instruction [] "vclzdm" (vclzdm @Interp)) x y


  xvmaddmdp :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xvmaddmdp x y z = {-# SCC "ISA.PowerCodeGraph.xvmaddmdp" #-}
    op3 @h (Instruction [] "xvmaddmdp" (xvmaddmdp @Interp)) x y z


  xvmsubmdp :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xvmsubmdp x y z = {-# SCC "ISA.PowerCodeGraph.xvmsubmdp" #-}
    op3 @h (Instruction [] "xvmsubmdp" (xvmsubmdp @Interp)) x y z

  xvnmaddmdp :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xvnmaddmdp x y z = {-# SCC "ISA.PowerCodeGraph.xvnmaddmdp" #-}
    op3 @h (Instruction [] "xvnmaddmdp" (xvnmaddmdp @Interp)) x y z

  xvnmsubmdp :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xvnmsubmdp x y z = {-# SCC "ISA.PowerCodeGraph.xvnmsubmdp" #-}
    op3 @h (Instruction [] "xvnmsubmdp" (xvnmsubmdp @Interp)) x y z

  vand :: Graph h VR -> Graph h VR -> Graph h VR   -- VN
  vand x y = {-# SCC "ISA.PowerCodeGraph.vand" #-}
    op2 @h (Instruction [] "vand" (vand @Interp)) x y

  xxland :: Graph h VR -> Graph h VR -> Graph h VR   -- VN
  xxland x y = {-# SCC "ISA.PowerCodeGraph.xxland" #-}
    op2 @h (Instruction [] "xxland" (xxland @Interp)) x y

  vor :: Graph h VR -> Graph h VR -> Graph h VR   -- VO
  vor x y = {-# SCC "ISA.PowerCodeGraph.vor" #-}
    op2 @h (Instruction [] "vor" (vor @Interp)) x y

  xxlor :: Graph h VR -> Graph h VR -> Graph h VR   -- VO
  xxlor x y = {-# SCC "ISA.PowerCodeGraph.vor" #-}
    op2 @h (Instruction [] "xxlor" (xxlor @Interp)) x y

  vorc :: Graph h VR -> Graph h VR -> Graph h VR   -- VNO
  vorc x y = {-# SCC "ISA.PowerCodeGraph.vorc" #-}
    op2 @h (Instruction [] "vorc" (vorc @Interp)) x y

  vxor :: Graph h VR -> Graph h VR -> Graph h VR   -- VX
  vxor x y = {-# SCC "ISA.PowerCodeGraph.vxor" #-}
    op2 @h (Instruction [] "vxor" (vxor @Interp)) x y

  vxorc :: Graph h VR -> Graph h VR -> Graph h VR
  vxorc x y = {-# SCC "ISA.PowerCodeGraph.vxorc" #-}
    op2 @h (Instruction [] "vxorc" (vxorc @Interp)) x y

  vandc :: Graph h VR -> Graph h VR -> Graph h VR   -- VNC
  vandc x y = {-# SCC "ISA.PowerCodeGraph.vandc" #-}
    op2 @h (Instruction [] "vandc" (vandc @Interp)) x y

  xvadddp :: Graph h VR -> Graph h VR -> Graph h VR
  xvadddp x y = {-# SCC "ISA.PowerCodeGraph.xvadddp" #-}
    op2 @h (Instruction [] "xvadddp" (xvadddp @Interp)) x y

  xvsubdp :: Graph h VR -> Graph h VR -> Graph h VR
  xvsubdp x y = {-# SCC "ISA.PowerCodeGraph.xvsubdp" #-}
    op2 @h (Instruction [] "xvsubdp" (xvsubdp @Interp)) x y

  xvmuldp :: Graph h VR -> Graph h VR -> Graph h VR
  xvmuldp x y = {-# SCC "ISA.PowerCodeGraph.xvmuldp" #-}
    op2 @h (Instruction [] "xvmuldp" (xvmuldp @Interp)) x y

--
  xvaddsp    :: Graph h VR -> Graph h VR -> Graph h VR
  xvaddsp x y = {-# SCC "ISA.PowerCodeGraph.xvaddsp" #-}
    op2 @h (Instruction [] "xvaddsp" (xvaddsp @Interp)) x y

  xvsubsp    :: Graph h VR -> Graph h VR -> Graph h VR
  xvsubsp x y = {-# SCC "ISA.PowerCodeGraph.xvsubsp" #-}
    op2 @h (Instruction [] "xvsubsp" (xvsubsp @Interp)) x y

  xvmulsp    :: Graph h VR -> Graph h VR -> Graph h VR
  xvmulsp x y = {-# SCC "ISA.PowerCodeGraph.xvmulsp" #-}
    op2 @h (Instruction [] "xvmulsp" (xvmulsp @Interp)) x y

  xvmaddmsp    :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xvmaddmsp x y z = {-# SCC "ISA.PowerCodeGraph.xvmaddmsp" #-}
      op_o3 @h (Instruction [] "xvmaddmsp" (xvmaddmsp @Interp)) (0,0) x y z

  xvnmaddmsp    :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xvnmaddmsp x y z = {-# SCC "ISA.PowerCodeGraph.xvnmaddmsp" #-}
      op_o3 @h (Instruction [] "xvnmaddmsp" (xvnmaddmsp @Interp)) (0,0) x y z

  xvmsubmsp    :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xvmsubmsp x y z = {-# SCC "ISA.PowerCodeGraph.xvmsubmsp" #-}
      op3 @h (Instruction [] "xvmsubmsp" (xvmsubmsp @Interp)) x y z

  xvnmsubmsp    :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  xvnmsubmsp x y z = {-# SCC "ISA.PowerCodeGraph.xvnmsubmsp" #-}
      op_o3 @h (Instruction [] "xvnmsubmsp" (xvnmsubmsp @Interp)) (0,0) x y z

  -- rotmai :: (Graph h VR) -> Integer -> (Graph h VR)
  -- rotmai x i = {-# SCC "ISA.PowerCodeGraph.rotmai" #-}
  --   op1 @h (Instruction [fromIntegral i] "rotmai"
  --           (\x0 -> rotmai @Interp x0 i)) x

  xvcvdpsp :: {-forall Graph h. PowerISA Graph h =>-} Graph h VR -> Graph h VR
  xvcvdpsp x = {-# SCC "ISA.PowerCodeGraph.xvcvdpsp" #-}
    op1 @h (Instruction [] "xvcvdpsp" (xvcvdpsp @Interp)) x

  vcfsx :: Graph h VR -> Integer -> Graph h VR
  vcfsx x i = {-# SCC "ISA.PowerCodeGraph.vcfsx" #-}
    op1 @h (Instruction [fromIntegral i] "vcfsx"
            (\x0 -> vcfsx @Interp x0 i)) x

  vcfux :: Graph h VR -> Integer -> Graph h VR
  vcfux x i = {-# SCC "ISA.PowerCodeGraph.vcfux" #-}
    op1 @h (Instruction [fromIntegral i] "vcfux"
            (\x0 -> vcfux @Interp x0 i)) x

  vctsxs :: Graph h VR -> Integer -> Graph h VR
  vctsxs x i = {-# SCC "ISA.PowerCodeGraph.vctsxs" #-}
    op1 @h (Instruction [fromIntegral i] "vctsxs"
            (\x0 -> vctsxs @Interp x0 i)) x

  vctuxs :: Graph h VR -> Integer -> Graph h VR
  vctuxs x i = {-# SCC "ISA.PowerCodeGraph.vctuxs" #-}
    op1 @h (Instruction [fromIntegral i] "vctuxs"
            (\x0 -> vctuxs @Interp x0 i)) x


  vrlwmi :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  vrlwmi x y z = {-# SCC "ISA.PowerCodeGraph.vrlwmi" #-}
    op3 @h (Instruction [] "vrlwmi" (vrlwmi @Interp)) x y z


    --  Overwrites first register
  vrlqmi :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  vrlqmi x y z = {-# SCC "ISA.PowerCodeGraph.vrlqmi" #-}
    op3 @h (Instruction [] "vrlqmi" (vrlqmi @Interp)) x y z

  vrlw :: Graph h VR -> Graph h VR -> Graph h VR
  vrlw x y = {-# SCC "ISA.PowerCodeGraph.vrlw" #-}
    op2 @h (Instruction [] "vrlw" (vrlw @Interp)) x y

  vrlq :: Graph h VR -> Graph h VR -> Graph h VR
  vrlq x y = {-# SCC "ISA.PowerCodeGraph.vrlq" #-}
    op2 @h (Instruction [] "vrlq" (vrlq @Interp)) x y



  vcmpgtub :: Graph h VR -> Graph h VR -> Graph h VR
  vcmpgtub x y = {-# SCC "ISA.PowerCodeGraph.vcmpgtub" #-}
    op2 @h (Instruction [] "vcmpgtub" (vcmpgtub @Interp)) x y

  -- vcmpgtuw :: Graph h VR -> Graph h VR -> Graph h VR
  -- vcmpgtuw x y = {-# SCC "ISA.PowerCodeGraph.vcmpgtuw" #-}
  --   op2 @h (Instruction [] "vcmpgtuw" (vcmpgtuw @Interp)) x y

  vcmpequw :: Graph h VR -> Graph h VR -> Graph h VR
  vcmpequw x y = {-# SCC "ISA.PowerCodeGraph.vcmpequw" #-}
    op2 @h (Instruction [] "vcmpequw" (vcmpequw @Interp)) x y

  vcmpgtfp :: Graph h VR -> Graph h VR -> Graph h VR
  vcmpgtfp x y = {-# SCC "ISA.PowerCodeGraph.vcmpgtfp" #-}
    op2 @h (Instruction [] "vcmpgtfp" (vcmpgtfp @Interp)) x y

  xvcmpgtsp :: Graph h VR -> Graph h VR -> Graph h VR
  xvcmpgtsp x y = {-# SCC "ISA.PowerCodeGraph.xvcmpgtsp" #-}
    op2 @h (Instruction [] "xvcmpgtsp" (xvcmpgtsp @Interp)) x y

  vcmpgefp :: Graph h VR -> Graph h VR -> Graph h VR
  vcmpgefp x y = {-# SCC "ISA.PowerCodeGraph.vcmpgefp" #-}
    op2 @h (Instruction [] "vcmpgefp" (vcmpgefp @Interp)) x y

  vcmpeqfp :: Graph h VR -> Graph h VR -> Graph h VR
  vcmpeqfp x y = {-# SCC "ISA.PowerCodeGraph.vcmpeqfp" #-}
    op2 @h (Instruction [] "vcmpeqfp" (vcmpeqfp @Interp)) x y

  vcmpbfp :: Graph h VR -> Graph h VR -> Graph h VR
  vcmpbfp x y = {-# SCC "ISA.PowerCodeGraph.vcmpbfp" #-}
    op2 @h (Instruction [] "vcmpbfp" (vcmpbfp @Interp)) x y

  vaddsws :: Graph h VR -> Graph h VR -> Graph h VR
  vaddsws x y = {-# SCC "ISA.PowerCodeGraph.vaddsws" #-}
    op2 @h (Instruction [] "vaddsws" (vaddsws @Interp)) x y

  vadduwm :: Graph h VR -> Graph h VR -> Graph h VR
  vadduwm x y = {-# SCC "ISA.PowerCodeGraph.vadduwm" #-}
    op2 @h (Instruction [] "vadduwm" (vadduwm @Interp)) x y

  vmulosh :: Graph h VR -> Graph h VR -> Graph h VR
  vmulosh x y = {-# SCC "ISA.PowerCodeGraph.vmulosh" #-}
    op2 @h (Instruction [] "vmulosh" (vmulosh @Interp)) x y

  vmuleuh :: Graph h VR -> Graph h VR -> Graph h VR
  vmuleuh x y = {-# SCC "ISA.PowerCodeGraph.vmuleuh" #-}
    op2 @h (Instruction [] "vmuleush" (vmuleuh @Interp)) x y

  -- vmulhsw :: Graph h VR -> Graph h VR -> Graph h VR
  -- vmulhsw x y = {-# SCC "ISA.PowerCodeGraph.vmulhsw" #-}
  --   op2 @h (Instruction [] "vmulhsw" (vmulhsw @Interp)) x y


  -- vector shift left
  vslw :: Graph h VR -> Graph h VR -> Graph h VR
  vslw x y = {-# SCC "ISA.PowerCodeGraph.vslw" #-}
    op2 @h (Instruction [] "vslw" (vslw @Interp)) x y
  -- vector shift right
  vsrw :: Graph h VR -> Graph h VR -> Graph h VR
  vsrw x y = {-# SCC "ISA.PowerCodeGraph.vsrw" #-}
    op2 @h (Instruction [] "vsrw" (vsrw @Interp)) x y
  -- vector shift right algebraic (sign extended)
  vsraw :: Graph h VR -> Graph h VR -> Graph h VR
  vsraw x y = {-# SCC "ISA.PowerCodeGraph.vsraw" #-}
    op2 @h (Instruction [] "vsraw" (vsraw @Interp)) x y

  vslq :: Graph h VR -> Graph h VR -> Graph h VR
  vslq x y = {-# SCC "ISA.PowerCodeGraph.vslq" #-}
    op2 @h (Instruction [] "vslq" (vslq @Interp)) x y

  ceqi :: Graph h VR -> Integer -> Graph h VR
  ceqi x i = {-# SCC "ISA.PowerCodeGraph.ceqi" #-}
    op1 @h (Instruction [fromIntegral i] "ceqi"
            (\x0 -> ceqi @Interp x0 i)) x

  -- mpya :: Graph h VR -> Graph h VR -> Graph h VR -> Graph h VR
  -- mpya x y z = {-# SCC "ISA.PowerCodeGraph.mpya" #-}
  --   op3 @h (Instruction [] "mpya" (mpya @Interp)) x y z

  -- mpyui :: Graph h VR -> Word32 -> Graph h VR
  -- mpyui x i = {-# SCC "ISA.PowerCodeGraph.mpyui" #-}
  --   op1 @h (Instruction [fromIntegral i] "mpyui"
  --           (\x0 -> mpyui @Interp x0 i)) x

  -- mpyi :: Graph h VR -> [Int32] -> Graph h VR
  -- mpyi x is = {-# SCC "ISA.PowerCodeGraph.mpyi" #-}
  --   op1 @h (Instruction (map fromIntegral is) "mpyi"
  --           (\x0 -> mpyi @Interp x0 is)) x


  -- mpy :: Graph h VR -> Graph h VR -> Graph h VR
  -- mpy x y = {-# SCC "ISA.PowerCodeGraph.mpy" #-}
  --   op2 @h (Instruction [] "mpy" (mpy @Interp)) x y

  --vcmpeqfp :: Graph h VR -> Graph h VR -> Graph h VR

  {- try to let recip work for inf and 0 on 23/06/2023-}
  --vcmpbfp :: Graph h VR -> Graph h VR -> Graph h VR
{-
    Vector Reciprocal Estimate Floating-Point VX-form
    recipSqrt14bit
-}
  vrefp :: Graph h VR -> Graph h VR
  vrefp x = {-# SCC "ISA.PowerCodeGraph.vrefp" #-}
    op1 @h (Instruction [] "vrefp" (vrefp @Interp)) x
{-
    Vector Reciprocal Square Root Estimate Floating-Point VX-form
    estimate recip of sqrt
-}
  vrsqrtefp :: Graph h VR -> Graph h VR
  vrsqrtefp x = {-# SCC "ISA.PowerCodeGraph.vrsqrtefp" #-}
    op1 @h (Instruction [] "vrsqrtefp" (vrsqrtefp @Interp)) x

{-
  Vector Insert Word from GPR using immediate-specified index VX-form
  insert [32:63] of gpr into vr using index
-}
  vinsw :: Graph h VR -> Graph h GPR -> Int -> Graph h VR
  vinsw x y i = {-# SCC "ISA.PowerCodeGraph.vinsw" #-}
    op2 @h (Instruction [fromIntegral i] "vinsw"
            (\x0 y0 -> vinsw @Interp x0 y0 i)) x y
