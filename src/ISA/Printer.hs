{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module         :   Coconut.Power.PowerISA
-- Copyright    :   (c) OCA 2021
-- License       :   MIT (see the LICENSE file)
-- Maintainer   :   curtis.dalves@gmail.com
-- Stability    :   provisional
-- Portability :   experimental
--
-- This module exports a DSL for the IBM Power Instruction Set Architecture (ISA), i.e,


module ISA.Printer where

import Coconut.BaseTypes
import ISA.PowerHardware

import qualified Numeric
import Data.Char (toLower)
import Data.ByteString (ByteString)
import Data.List (elemIndex)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.Word (Word64, Word32, Word16, Word8)
import Data.Time (TimeLocale(wDays))

constantLabel = "T.22.constants"

instance Printer POWER where
  data InstructionFormat POWER =
      RRtoR
    | RItoR
    | RtoR
    | RDtoR
    | RIItoR
    | RRIItoR
    | RRItoR
    | RRIIItoR
    | RRRtoR
    | RROtoR
    | DQForm
    | FDSForm
    | LabelForm
    | CmpBranchLabel
    | ConstDecodeGPR
    | ConstDecodeVR
    | PointerLabelLoad
    --  VVtoV
    --  VItoV
    --  GtoG
    --  GGtoG
    --  VItoV
    --  GIItoG
    --  GGIItoG

  printableInstrLabel = mdPowerPName
  printableInstrFormat = mdPowerConv
  printableInstruction metaData mFormat name regs imms label cTable = let
      regs' = decrementNonVSX name metaData regs -- On newer versions of POWER, old vector
                                                 -- instructions (i.e., nonVSX) can only use registers
                                                 -- 32-63, but the assembler chooses to
                                                 -- have the user write registers 0-31, and then
                                                 -- automatically increments them by 32.
                                                 -- So when generating assembly code, we need to
                                                 -- detect all non VSX instructions, and decrement their
                                                 -- vector registers by 32
    in instructionFormatPrint mFormat name regs' imms label cTable
  printableTable _cg label pairs = let
    -- TODO find a way to adjust RO/RW (READ ONLY / READ WRITE) and alignment
    header0 = "    .csect " <> BS.pack (map toLower label) <> "[RW], 3"
    header1 = "    .globl "  <> BS.pack (map toLower label) <> "[RW]"
    header2 = "    .align 2"

    genHex x = let h = Numeric.showHex x "" in "0x" <> (replicate (16-length h)'0') <> h
    genPair (a,b) = ["    .long " <> BS.pack (genHex a)
                    ,"    .long " <> BS.pack (genHex b)]
    toHexLine x = let
      h = Numeric.showHex x ""
      h' = (replicate (16-length h)'0') <> h
      h0 = "0x" <> drop 8 h'
      h1 = "0x" <> take 8 h'
      in ["    .vbyte 4, " <> BS.pack h1
         ,"    .vbyte 4, " <> BS.pack h0]
    unPair = concatMap (\(x,y) -> [x,y])
    in header0 : header1 : header2 : (concatMap toHexLine $ unPair pairs)

  printableSectionLabel _cg label = BS.pack label <> ":"

instructionFormatPrint :: Maybe (InstructionFormat POWER)
  -> ByteString
  -> [(RegType POWER, ByteString)]
  -> [Int]
  -> Maybe String
  -> [(Word64, Word64)]
  -> ByteString

instructionFormatPrint (Just RRtoR) name [(type0, r0), (type1, r1), (type2, r2)] [] label cTable
  = name <> " " <> regPrefix type2 <> r2 <> "," <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1

instructionFormatPrint (Just RItoR) name [(type0, r0), (type1, r1)] [uim] label cTable
  = name <> " " <> regPrefix type1 <> r1 <> "," <> regPrefix type0 <> r0 <> "," <> toBS uim

instructionFormatPrint (Just RtoR) name [(type0, r0), (type1, r1)] [] label cTable
  = name <> " " <> regPrefix type1 <> r1 <> "," <> regPrefix type0 <> r0

instructionFormatPrint (Just RDtoR) name [(type0, r0), (type1, r1)] [] label cTable
  = name <> " " <> regPrefix type1 <> r1 <> "," <> regPrefix type0 <> r0 <> "," <> regPrefix type0 <> r0

instructionFormatPrint (Just RIItoR) name [(type0, r0), (type1, r1)] [uim1, uim2] label cTable
  = name <> " " <> regPrefix type1 <> r1 <> "," <> regPrefix type0 <> r0 <> "," <> toBS uim1 <> "," <> toBS uim2

instructionFormatPrint (Just RRIItoR) name [(type0, r0), (type1, r1), (type2, r2)] [uim1, uim2] label cTable
  = name <> " " <> regPrefix type2 <> r2 <> "," <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1 <> "," <> toBS uim1 <> "," <> toBS uim2

instructionFormatPrint (Just RRItoR) name [(type0, r0), (type1, r1), (type2, r2)] [uim1] label cTable
  = name <> " " <> regPrefix type2 <> r2 <>"," <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1 <> "," <> toBS uim1

instructionFormatPrint (Just RRItoR) name [(type0, r0), (type1, r1), (type2, r2)] [uim1, uim2, uim3] label cTable
  = name <> " " <> regPrefix type2 <> r2 <> "," <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1 <> "," <> toBS uim1 <> toBS uim2 <> toBS uim3

instructionFormatPrint (Just RRRtoR) name [(type0, r0), (type1, r1), (type2, r2), (type3, r3)] [] label cTable
  = name <> " " <> regPrefix type3 <> r3 <> "," <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1 <> "," <> regPrefix type2 <> r2

instructionFormatPrint (Just RROtoR) name [(_type0, _r0), (type1, r1), (type2, r2), (type3, r3)] [] label cTable
  = name <> " " <> regPrefix type3 <> r3 <> "," <> regPrefix type1 <> r1 <> "," <> regPrefix type2 <> r2

instructionFormatPrint (Just DQForm) name [(type0, r0), (type1, r1)] [uim1] label cTable
  = name <> " " <> regPrefix type1 <> r1 <> "," <> toBS uim1 <> "(" <> regPrefix type0 <> r0 <> ")"
instructionFormatPrint (Just DQForm) name [(type0, r0), (type1, r1),(type2,r2)] [uim1] label cTable
  = name <> " " <> regPrefix type1 <> r1 <> "," <> toBS uim1 <> "(" <> regPrefix type0 <> r0 <> ")"
-- NOTE this DQForm is used to handle moduloVStore/moduloVLoad, which doesnt use the first MR input
instructionFormatPrint (Just DQForm) name [(type0, r0), (type1, r1),(type2,r2),(type3,r3)] [uim1] label cTable
  = name <> " " <> regPrefix type2 <> r2 <> "," <> toBS uim1 <> "(" <> regPrefix type1 <> r1 <> ")"

instructionFormatPrint (Just FDSForm) name [(type0, x2), (type1, b2), (type2, v1), _] [d2] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> toBS d2 <> "(" <> regPrefix type0 <> b2 <> ")"

instructionFormatPrint (Just LabelForm) name [] [] (Just label) cTable
  = name <> " " <> BS.pack label

instructionFormatPrint (Just CmpBranchLabel) name [(type0,r0),(type1,r1)] [] (Just label) cTable
  = "cmp 0," <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1 <> " ; "<> name <> " " <> BS.pack label
instructionFormatPrint (Just CmpBranchLabel) name [(type0,r0)] [uim1] (Just label) cTable
  = "cmpi 0," <> regPrefix type0 <> r0 <> "," <> toBS uim1 <> " ; "<> name <> " " <> BS.pack label

instructionFormatPrint (Just ConstDecodeVR) name [(type0, v)] [imm1, imm2] label cTable = let
  mDisp = elemIndex (fromIntegral imm1, fromIntegral imm2) cTable
  in case mDisp of
    Nothing -> error $ "instructionFormatPrint on " ++ show name ++ " can't find immediate" ++ show (imm1,imm2)
    Just idx -> "lxv" <> " " <> regPrefix type0 <> v <> "," <> toBS (idx * 16)
                     <> "(r6)"
instructionFormatPrint (Just ConstDecodeGPR) name [(type0, g)] [imm] label cTable = let
  mDisp = elemIndex (fromIntegral imm, fromIntegral imm) cTable
  in case mDisp of
    Nothing -> error $ "instructionFormatPrint on " ++ show name ++ " can't find immediate" ++ show imm
    Just idx -> "ld" <> " " <> regPrefix type0 <> g <> "," <> toBS (idx * 16)
                     <> "(r6)"

instructionFormatPrint (Just PointerLabelLoad) name [(type0,r0)] [] (Just "CONSTANTS") cTable
  = "ld " <> regPrefix type0 <> r0 <> ",L..constants(RTOC)"
 -- TODO compute displacement from stack instead of hardcoding
instructionFormatPrint (Just PointerLabelLoad) name [(type0,r0)] [] (Just "SCRATCH") cTable
  = "la " <> regPrefix type0 <> r0 <> ",-624(SP)" -- SP is r1 is the stack pointer
-- TODO figure out how to handle labels with TOC for other MRs besides constants
instructionFormatPrint (Just PointerLabelLoad) name [(type0,r0)] [] (Just label) cTable
  = "ld " <> regPrefix type0 <> r0 <> "," <> BS.pack label

instructionFormatPrint Nothing _ _ _ _ _ = ""
-- TODO: better error handling? We will want to see length of arg/imm lists
instructionFormatPrint _ name regList immList label cTable =
  "error in instructionFormatPrint: " <> name <> " where\n\t regList = " <> toBS (show regList)
                                              <> "\n\t immList = " <> toBS (show immList)

toBS :: (Show a) => a -> ByteString
toBS = BS.pack . show


decrementNonVSX :: ByteString -> MDMap POWER -> [(RegType POWER, ByteString)] -> [(RegType POWER, ByteString)]
decrementNonVSX name metaData regs = let
    name' = BS.unpack name
    isNonVSX = case Map.lookup name' $ mdMap metaData of
                 Just mData -> mdIsNonVSX mData
                 Nothing -> False -- TODO this should throw an error (why does initMR (i.e. l) get called here)
                                  -- error $ "instruction name not in metaData map: " ++ name'
  in
  if isNonVSX
  then map (\(t,r) -> if t == VR
                      then (t,nonVSXDecrement r)
                      else (t,r)) regs
  else regs

nonVSXDecrement :: ByteString -> ByteString
nonVSXDecrement bs = case bs of
                       "32" -> "0"
                       "33" -> "1"
                       "34" -> "2"
                       "35" -> "3"
                       "36" -> "4"
                       "37" -> "5"
                       "38" -> "6"
                       "39" -> "7"
                       "40" -> "8"
                       "41" -> "9"
                       "42" -> "10"
                       "43" -> "11"
                       "44" -> "12"
                       "45" -> "13"
                       "46" -> "14"
                       "47" -> "15"
                       "48" -> "16"
                       "49" -> "17"
                       "50" -> "18"
                       "51" -> "19"
                       "52" -> "20"
                       "53" -> "21"
                       "54" -> "22"
                       "55" -> "23"
                       "56" -> "24"
                       "57" -> "25"
                       "58" -> "26"
                       "59" -> "27"
                       "60" -> "28"
                       "61" -> "29"
                       "62" -> "30"
                       "63" -> "31"
                       _ -> error $ "nonVSXDecrement given bad register: " ++ BS.unpack bs
