-- |
-- Module      :  Coconut.Core.MetaData
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality generating a implementing CORE hardware functionality

{-# LANGUAGE OverloadedStrings #-}
module ISA.MetaData where

import Coconut.BaseTypes (MDMap (..), Interp, runInterpVR)
import ISA.PowerHardware (POWER,MetaData(..))
import ISA.Printer (InstructionFormat(..))
import ISA.PowerISA
import ISA.PowerInterp
import Coconut.Core.CoreISA
import Coconut.Core.Interp

import qualified Data.Map as Map


powerMetaData :: MDMap POWER
powerMetaData = MDMap $ Map.fromList
  [("undwrds", MDPower "undwrds" False 5 (const ConstDecodeVR))
  ,("unwrds", MDPower "unwrds" False 5 (const ConstDecodeVR))
  ,("unbytes", MDPower "unbytes" False 5 (const ConstDecodeVR))
  ,("unshorts", MDPower "unshorts" False 5 (const ConstDecodeVR))
  ,("undoubles", MDPower "undoubles" False 5 (const ConstDecodeVR))
  ,("uninteger", MDPower "uninteger" False 5 (const ConstDecodeVR))
  ,("unintegerG",MDPower "unintegerG" False 5 (const ConstDecodeGPR))
  ,("unintegerG",MDPower "unintegerG" False 5 (const ConstDecodeGPR))
  ,("unfloats",MDPower "unfloats" False 5 (const ConstDecodeVR))
  ,("unfloats4",MDPower "unfloats4" False 5 (const ConstDecodeVR))
  ,("unint32s",MDPower "unint32s" False 5 (const ConstDecodeVR))
  ,("initMR", MDPower "l" False 3 (const PointerLabelLoad))
  ,("incMR", MDPower "addc" False 3 (const RRtoR ))
  ,("mr", MDPower "mr" False 3 (const RtoR))
  ,("vmr", MDPower "xxlor" False 3 (const RDtoR)) -- TODO is there no xvmr? what if you want to move the lower VSR registers?
  --,("xvmr", MDPower "xxlor" True 3 (const RDtoR))
  ,("xxlor", MDPower "xxlor" False 3 (const RRtoR ))
  ,("absG", MDPower "absG" False 3 (const RtoR ))
  ,("addcG", MDPower "addc" False 3 (const RRtoR ))
  ,("addG", MDPower "addc" False 3 (const RRtoR ))
  ,("addiG", MDPower "addiG" False 3 (const RItoR ))
  ,("subG", MDPower "sub" False 3 (const RRtoR ))
  ,("subfG", MDPower "subf" False 3 (const RRtoR ))
  ,("subficG", MDPower "subficG" False 3 (const RItoR ))
  ,("negG", MDPower "negG" False 3 (const RtoR ))
  ,("mulhdG", MDPower "mulhdG" False 3 (const RRtoR ))
  ,("mulldG", MDPower "mulldG" False 3 (const RRtoR ))
  ,("mulhduG", MDPower "mulhduG" False 3 (const RRtoR ))
  ,("divdG", MDPower "divdG" False 3 (const RRtoR ))
  ,("divduG", MDPower "divduG" False 3 (const RRtoR ))
  ,("rldiclG", MDPower "rldiclG" False 3 (const RIItoR ))
  ,("rldicrG", MDPower "rldicrG" False 3 (const RIItoR ))
  ,("rldimiG", MDPower "rldimiG" False 3 (const RRIItoR ))
  ,("rldicG", MDPower "rldicG" False 3 (const RIItoR ))
  ,("rldclG", MDPower "rldclG" False 3 (const RRItoR ))
  ,("rldcrG", MDPower "rldcrG" False 3 (const RRItoR ))
  ,("rlwimiG", MDPower "rlwimiG" False 3 (const RRIIItoR ))
  ,("add", MDPower "add" False 3 (const RRtoR ))
  ,("xorG", MDPower "xor" False 3 (const RRtoR ))
  ,("xoriG", MDPower "xori" False 3 (const RItoR ))
  ,("eqvG", MDPower "eqvG" False 3 (const RRtoR ))
  ,("nandG", MDPower "nandG" False 3 (const RRtoR ))
  ,("orG", MDPower "orG" False 3 (const RRtoR ))
  ,("norG", MDPower "norG" False 3 (const RRtoR ))
  ,("cmp", MDPower "cmp" False 3 (const RRRtoR ))
  ,("divdu", MDPower "divdu" False 3 (const RRtoR ))
  ,("andG", MDPower "andG" False 3 (const RRtoR ))
  ,("andc", MDPower "andc" False 3 (const RRtoR ))
  ,("eqv", MDPower "eqv" False 3 (const RRtoR ))
  ,("cntlzd", MDPower "cntlzd" False 3 (const RtoR ))
  ,("cntlzw", MDPower "cntlzw" False 3 (const RtoR ))
  ,("vsel", MDPower "vsel" True 3 (const RRRtoR ))
  ,("xxsel", MDPower "xxsel" False 3 (const RRRtoR ))
  ,("vperm", MDPower "vperm" True 3 (const RRRtoR ))
  ,("vpermr", MDPower "vpermr" True 3 (const RRRtoR ))
  ,("vspltb", MDPower "vspltb" True 3 (const RItoR ))
  ,("vmrghb", MDPower "vmrghb" True 3 (const RRtoR ))
  ,("vmrglb", MDPower "vmrglb" True 3 (const RRtoR ))
  ,("vmrghh", MDPower "vmrghh" True 3 (const RRtoR ))
  ,("vmrglh", MDPower "vmrglh" True 3 (const RRtoR ))
  ,("vmrghw", MDPower "vmrghw" True 3 (const RRtoR ))
  ,("vmrglw", MDPower "vmrglw" True 3 (const RRtoR ))
  ,("vmrgew", MDPower "vmrgew" True 3 (const RRtoR ))
  ,("vmrgow", MDPower "vmrgow" True 3 (const RRtoR ))
  ,("vclzb", MDPower "vclzb" True 3 (const RtoR ))
  ,("vclzh", MDPower "vclzh" True 3 (const RtoR ))
  ,("vclzw", MDPower "vclzw" True 3 (const RtoR ))
  ,("vclzd", MDPower "vclzd" True 3 (const RtoR ))
  ,("vclzdm", MDPower "vclzdm" True 3 (const RRtoR ))
  ,("xvmaddmdp", MDPower "xvmaddmdp" False 3 (const RRRtoR ))
  ,("xvmsubmdp", MDPower "xvmsubmdp" False 3 (const RRRtoR ))
  ,("xvnmaddmdp", MDPower "xvnmaddmdp" False 3 (const RRRtoR ))
  ,("xvnmsubmdp", MDPower "xvnmsubmdp" False 3 (const RRRtoR ))
  ,("vand", MDPower "vand" True 3 (const RRtoR ))
  ,("xxland", MDPower "xxland" False 3 (const RRtoR ))
  ,("vor", MDPower "vor" True 3 (const RRtoR ))
  ,("vorc", MDPower "vorc" True 3 (const RRtoR ))
  ,("vxor", MDPower "vxor" True 3 (const RRtoR ))
  ,("vxorc", MDPower "vxorc" True 3 (const RRtoR ))
  ,("vandc", MDPower "vandc" True 3 (const RRtoR ))
  ,("xvadddp", MDPower "xvadddp" False 3 (const RRtoR ))
  ,("xvsubdp", MDPower "xvsubdp" False 3 (const RRtoR ))
  ,("xvmuldp", MDPower "xvmuldp" False 3 (const RRtoR ))
  ,("xvaddsp", MDPower "xvaddsp" False 3 (const RRtoR ))
  ,("xvsubsp", MDPower "xvsubsp" False 3 (const RRtoR ))
  ,("xvmulsp", MDPower "xvmulsp" False 3 (const RRtoR ))
  ,("xvmaddmsp", MDPower "xvmaddmsp" False 3 (const RROtoR ))
  ,("xvmsubmsp", MDPower "xvmsubmsp" False 3 (const RRRtoR ))
  ,("xvnmaddmsp", MDPower "xvnmaddmsp" False 3 (const RROtoR ))
  ,("xvnmsubmsp", MDPower "xvnmsubmsp" False 3 (const RROtoR ))
  ,("xvcvdpsp", MDPower "xvcvdpsp" False 3 (const RtoR ))
  ,("vcfsx", MDPower "vcfsx" True 3 (const RItoR ))
  ,("vcfux", MDPower "vcfux" True 3 (const RItoR ))
  ,("vctsxs", MDPower "vctsxs" True 3 (const RItoR ))
  ,("vctuxs", MDPower "vctuxs" True 3 (const RItoR ))
  ,("vrlwmi", MDPower "vrlwmi" True 3 (const RRRtoR ))
  ,("vrlqmi", MDPower "vrlqmi" True 3 (const RRtoR ))
  ,("vrlw", MDPower "vrlw" True 3 (const RRtoR ))
  ,("vrlq", MDPower "vrlq" True 3 (const RRtoR ))
  ,("vcmpgtub", MDPower "vcmpgtub" True 3 (const RRtoR ))
  ,("vcmpgtuw", MDPower "vcmpgtuw" True 3 (const RRtoR ))
  ,("vcmpequw", MDPower "vcmpequw" True 3 (const RRtoR ))
  ,("vcmpgtfp", MDPower "vcmpgtfp" True 3 (const RRtoR ))
  ,("xvcmpgtsp", MDPower "xvcmpgtsp" False 3 (const RRtoR ))
  ,("vcmpgefp", MDPower "vcmpgefp" True 3 (const RRtoR ))
  ,("vcmpeqfp", MDPower "vcmpeqfp" True 3 (const RRtoR ))
  ,("vcmpbfp", MDPower "vcmpbfp" True 3 (const RRtoR ))
  ,("vaddsws", MDPower "vaddsws" True 3 (const RRtoR ))
  ,("vadduwm", MDPower "vadduwm" True 3 (const RRtoR ))
  ,("vsubuwm", MDPower "vsubuwm" True 3 (const RRtoR ))
  ,("vmodsw", MDPower "vmodsw" True 3 (const RRtoR ))
  ,("vnegw", MDPower "vnegw" True 3 (const RtoR ))
  ,("vmulosh", MDPower "vmulosh" True 3 (const RRtoR ))
  ,("vmuleuh", MDPower "vmuleuh" True 3 (const RRtoR ))
  ,("vmulhsw", MDPower "vmulhsw" True 3 (const RRtoR ))
  ,("vslw", MDPower "vslw" True 3 (const RRtoR ))
  ,("vsrw", MDPower "vsrw" True 3 (const RRtoR ))
  ,("vsraw", MDPower "vsraw" True 3 (const RRtoR ))
  ,("vslq", MDPower "vslq" True 3 (const RRtoR ))
  ,("ceqi", MDPower "ceqi" False 3 (const RItoR )) -- TODO does this exist?
  ,("mpya", MDPower "mpya" False 3 (const RRRtoR )) -- TODO does this exist?
  ,("mpyui", MDPower "mpyui" False 3 (const RItoR )) -- TODO does this exist?
  ,("mpyi", MDPower "mpyi" False 3 (const RItoR )) -- TODO does this exist?
  ,("mpy", MDPower "mpy" False 3 (const RRtoR )) -- TODO does this exist?
  ,("vrefp", MDPower "vrefp" True 3 (const RtoR ))
  ,("vrsqrtefp", MDPower "vrsqrtefp" True 3 (const RtoR ))
  ,("vinsw", MDPower "vinsw" True 3 (const RRItoR ))
  ,("lvx", MDPower "lvx" True 3 (const RRtoR))
  ,("stvx", MDPower "stvx" True 3 (const RRtoR))
  ,("lxv", MDPower "lxv" False 3 (const DQForm))
  ,("stxv", MDPower "stxv" False 3 (const DQForm))
  ,("jump",MDPower "b" False 3 (const LabelForm))
  ,("branchLow",MDPower "blt" False 3 (const CmpBranchLabel))
  ,("branchNotLow",MDPower "bge" False 3 (const CmpBranchLabel))
  ,("branchImmNotEq",MDPower "bne" False 3 (const CmpBranchLabel))
  ,("branchImmNotHigh",MDPower "ble" False 3 (const CmpBranchLabel))
 -- ,("spillGPR",error "TODO implement spillGPR in powerMetaData")
  -- ,("despillGPR",error "TODO implement despillGPR in powerMetaData")
--  ,("spillVR",error "TODO implement spillVR in powerMetaData")
  -- ,("despillVR",error "TODO implement despillVR in powerMetaData")
  ,("moduloDWLoad",MDPower "ld" False 3 (const FDSForm))
  ,("moduloDWStore",MDPower "std" False 3 (const FDSForm))
  ,("moduloVLoad", MDPower "lxv" False 3 (const DQForm))
  ,("moduloVStore", MDPower "stxv" False 3 (const DQForm))
  ]
