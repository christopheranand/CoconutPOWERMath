{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Module         :   Coconut.Power.PowerISA
-- Copyright    :   (c) OCA 2021
-- License       :   MIT (see the LICENSE file)
-- Maintainer   :   curtis.dalves@gmail.com
-- Stability    :   provisional
-- Portability :   experimental
--
-- This module exports a DSL for the IBM Power Instruction Set Architecture (ISA), i.e,

module ISA.PowerISA (module ISA.PowerISA, module Coconut.Core.CoreISA) where

import Data.Word (Word64, Word32, Word16, Word8)
import qualified Data.List as List
import qualified Data.ByteString as BS
import GHC.Arr (inRange)
import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Numeric (showHex)
import Data.List (intercalate)

import Coconut.BaseTypes
import Coconut.Core.CoreISA

import Coconut.Utils.RunTimeSized
import Data.Int (Int64, Int8, Int16, Int32)
import Coconut.Utils.ArbFloat (dbl2Word32, dbl2Word64, chop, word322Dbl, word642Dbl, FPClass(..), af2DVal, dval2af)

import Data.Bits as Bits (xor, rotateL, complement, shift, shiftR, shiftL, (.&.), rotate, (.|.))
import Data.List.Split (chunksOf)
import Coconut.Utils.ArbFloat

-- | Finally tagless style DSL for the Z Coconut Instruction Set Architecture (ISA)
class CoreISA repr => PowerISA repr where

-- || Constants || --
   signBit, maxFloat, minFloat, maxNegFloat, minNegFloat :: PowerISA repr => repr VR
   signBit = unwrds4 0x80000000
   maxFloat = error " this doesn't exist in IEEE floats, if we use it, it must be removed unwrds4 0x7fffffff"
   minFloat = error " this doesn't exist in IEEE floats, if we use it, it must be removedunwrds4 0x00800000"
   maxNegFloat = error " this doesn't exist in IEEE floats, if we use it, it must be removedunwrds4 0xffffffff"
   minNegFloat = error " this doesn't exist in IEEE floats, if we use it, it must be removedunwrds4 0x80800000"

-- || FUNCTIONS TO CONVERT OTHER TYPES TO REGISTERS || --

   -- | Stores an signed integer value in a general purpose register.
   unintegerSG :: Integer -> repr GPR

   -- | Stores a list of integers in a vector register.
   unwrds'       :: [Word32] -> repr VR
   unwrds' = unwrds . map fromIntegral

   -- | Stores a list of single-precision floating point numbers in a vector register 4 times.
   unfloats4    :: Double -> repr VR
   unfloats4 = unfloats . (replicate 4)

-- || Instructions for Loads / Stores || --

   -- | Loads a vector from address RA (repr MR) and offset RB (repr GPR)
   lvx :: repr MR -> repr GPR -> (repr VR, repr MR)

   -- | Stores a vector from address RA (repr MR) and offset RB (repr GPR)
   stvx :: repr MR -> repr GPR -> repr VR -> repr MR

   -- | Load VSX Vector DQ-form (store with immediate offset)
   lxv :: repr MR -> Int -> (repr VR,repr MR)

   -- | Store VSX Vector DQ-form (store with immediate offset)
   stxv :: repr MR -> Int -> repr VR -> repr MR

-- || FUNCTIONS TO DO ARITHMETIC || --

   -- | Takes the absolute value of the contents of a general-purpose register and places the result in another general-purpose register.
   -- | Name: Absolute
   -- | Function: abs
   absG :: repr GPR -> repr GPR

   -- | Adds the contents of two general-purpose registers and places the result in a general-purpose register
   -- | Name: Add Carrying
   -- | Function: addc
   addcG :: repr GPR -> repr GPR -> repr GPR

   -- | Calculates an address from an offset and a base address and places the result in a general-purpose register.
   -- | Name: Add Immediate / Compute Address Lower
   -- | Function: addi / cal
   addiG :: repr GPR -> Int -> repr GPR

   -- | Subtracts the contents of two general-purpose registers and places the result in a third general-purpose register.
   -- | Name: Subtract From
   -- | Function: subf
   subfG    :: repr GPR -> repr GPR -> repr GPR

   -- | Subtracts the contents of a general-purpose register from a 16-bit signed integer and places the result in another general-purpose register.
   -- | Name: Subtract from Immediate Carrying
   -- | Function: subfic / sfi
   subficG    :: repr GPR -> Int -> repr GPR

   -- | Negation of a 64-bit general-purpose register
   -- | Name: Negate
   -- | Function: neg
   negG      :: repr GPR -> repr GPR

   -- | Signed multiplication of two 64-bit values. Place the high-order 64 bits of the result into a register.
   -- | Name: Multiply High Double Word
   -- | Function: mulhd
   mulhdG   :: repr GPR -> repr GPR -> repr GPR

   -- | Signed multiplcation of two 64-bit general-purpose registers. Place the low-order 64 bits of the result into a register.
   -- | Name: Multiply Low Double Word
   -- | Function: mulld
   mulldG   :: repr GPR -> repr GPR -> repr GPR

   -- | Unsigned multiplication of two general-purpose registers. Place the high-order 64 bits of the result into a register.
   -- | Name: Multiply High Double Word Unsigned
   -- | Function: mulhdu
   mulhduG :: repr GPR -> repr GPR-> repr GPR

   -- | Signed division of the contents of a general purpose register by the contents of a general purpose register, storing the result into a general purpose register.
   -- | Name: Divide Double Word
   -- | Function: divd
   divdG    :: repr GPR -> repr GPR -> repr GPR

   -- | Unsigned division of the contents of a general purpose register by the contents of a general purpose register, storing the result into a general purpose register.
   -- | Name: Divide Double Word Unsigned
   -- | Function: divdu
   divduG   :: repr GPR -> repr GPR -> repr GPR


-- ******GO THROUGH THESE AND MAKE SURE THEY MAKE SENSE *************
   -- | Rotate the contents of a general purpose register left by the number of bits specified by an immediate value. Clear a specified number of high-order bits. Place the results in another general purpose register.
   -- | Name: Rotate Left Double Word Immediate then Clear Left
   -- | Function: rldicl
   rldiclG :: repr GPR -> Integer -> Integer -> repr GPR

   -- | TODO: find equivalent instruction, if any
    -- | Rotate the contents of a general purpose register left by the number of bits specified by an immediate value. Clear a specified number of low-order bits. Place the results in another general purpose register.
    -- | Name: Rotate Left Double Word Immediate then Clear Right
    -- | Function: rldicr
   rldicrG :: repr GPR -> Integer -> Integer -> repr GPR

   -- | TODO: find equivalent instruction, if any
    -- | The contents of a general purpose register are rotated left a specified number of bits. A generated mask is used to insert a specified bit-field into the corresponding bit-field of another general purpose register.
    -- | Name: Rotate Left Double Word Immediate then Mask Insert
    -- | Function: rldimi
   rldimiG :: repr GPR {- inOut -} -> repr GPR -> Integer -> Integer -> repr GPR

   -- | TODO: find equivalent instruction, if any
    -- | The contents of a general purpose register are rotated left a specified number of bits, then masked with a bit-field to clear some number of low-order and high-order bits. The result is placed in another general purpose register.
    -- | Rotate Left Double Word Immediate then Clear
    -- | Function: rldic
   rldicG   :: repr GPR -> Integer -> Integer -> repr GPR

   -- | TODO: find equivalent instruction, if any
    -- | Rotate the contents of a general purpose register left by the number of bits specified by the contents of another general purpose register. Generate a mask that is ANDed with the result of the shift operation. Store the result of this operation in another general purpose register.
    -- | Rotate Left Double Word then Clear Left
    -- | Function: rldcl
   rldclG   :: repr GPR -> repr GPR -> Integer -> repr GPR

   -- | TODO: find equivalent instruction, if any
    -- | Rotate the contents of a general purpose register left by the number of bits specified by the contents of another general purpose register. Generate a mask that is ANDed with the result of the shift operation. Store the result of this operation in another general purpose register.
    -- | Rotate Left Double Word then Clear Right
    -- | Function: rldcr
   rldcrG   :: repr GPR -> repr GPR -> Integer -> repr GPR

   -- | TODO: find equivalent instruction, if any
    -- | Rotates the contents of a general-purpose register to the left by a specified number of bits and stores the result in another general-purpose register under the control of a generated mask.
    -- | Rotate Left Word Immediate Then Mask Insert
    -- | Function: rlwimi / rlimi
   rlwimiG :: repr GPR {- inOut -} -> repr GPR -> Integer -> Integer -> Integer -> repr GPR

   -- | Adds the contents of two general-purpose registers
   -- | Name: Add
   -- | Function: add
   add :: repr GPR -> repr GPR -> repr GPR

   -- | XORs the lower 16 bits of a general-purpose register with a 16-bit unsigned integer and places the result in another general-purpose register
   -- | Name: xori
   -- | Function: xoriG
   xoriG :: repr GPR -> Word -> repr GPR

   -- | Logically XORs the contents of two general-purpose registers and places the complemented result in a general-purpose register
   -- | Name: eqv
   -- | Function: eqvG
   eqvG :: repr GPR -> repr GPR -> repr GPR

   -- | Logically complements the result of ANDing the contents of two general-purpose registers and stores the result in another general-purpose register
   -- | Name: nand
   -- | Function: nandG
   nandG :: repr GPR -> repr GPR -> repr GPR

   -- | Logically ORs the contents of two general-purpose registers and stores the result in another general-purpose register
   -- | Name: or
   -- | Function: orG
   -- orG :: repr GPR -> repr GPR -> repr GPR

   -- | Logically complements the result of ORing the contents of two general-purpose registers and stores the result in another general-purpose register
   -- | Name: nor
   -- | Function: norG
   norG :: repr GPR -> repr GPR -> repr GPR

   -- | XORs the upper 16 bits of a general-purpose register with a 16-bit unsigned integer and places the result in another general-purpose register
   -- | Name: xoris
   -- | Function: xorisG
   -- xorisG :: repr GPR -> Int -> repr GPR

   -- | Compares the contents of two general-purpose registers algebraically
   -- | Name: cmp
   -- | Function: cmp
   cmp :: repr GPR -> repr GPR -> repr GPR -> repr GPR

   -- | Divide the contents of a general purpose register by the contents of a general purpose register, storing the result into a general purpose register
   -- | Name: divdu
   -- | Function: divdu
   divdu :: repr GPR -> repr GPR -> repr GPR

   -- | Logically ANDs the contents of a general-purpose register with the complement of the contents of a general-purpose register
   -- | Name: andc
   -- | Function: andc
   andc :: repr GPR -> repr GPR -> repr GPR

   -- | Places the complemented result of XORing two Condition Register bits in a Condition Register bit
   -- | Name: creqv
   -- | Function: creqv
   creqv :: repr CR -> repr CR -> repr CR

   -- | Places the result of ANDing two Condition Register bits in a Condition Register bit
   -- | Name: crand
   -- | Function: crand
   crand :: repr CR -> repr CR -> repr CR

   -- | Places the result of ANDing one Condition Register bit and the complement of a Condition Register bit in a Condition Register bit
   -- | Name: crandc
   -- | Function: crandc
   crandc :: repr CR -> repr CR -> repr CR

   -- | Places the complemented result of ANDing two Condition Register bits in a Condition Register bit
   -- | Name: crnand
   -- | Function: crnand
   crnand :: repr CR -> repr CR -> repr CR

   -- | Places the result of ORing two Condition Register bits in a Condition Register bit
   -- | Name: crnor
   -- | Function: crnor
   crnor :: repr CR -> repr CR -> repr CR

   -- | Places the result of XORing two Condition Register bits in a Condition Register bit
   -- | Name: crxor
   -- | Function: crxor
   crxor :: repr CR -> repr CR -> repr CR

   -- | Places the result of ORing a Condition Register bit and the complement of a Condition Register bit in a Condition Register bit
   -- | Name: crorc
   -- | Function: crorc
   crorc :: repr CR -> repr CR -> repr CR

   -- | Logically XORs the contents of two general-purpose registers and places the complemented result in a general-purpose register
   -- | Name: eqv
   -- | Function: eqv
   eqv :: repr GPR -> repr GPR -> repr GPR


-- || FUNCTIONS TO COUNT LEADING ZEROS || --

   -- | Count the number of consecutive zero bits in the contents of a general purpose register, beginning with the high-order bit.
   -- | Function: cntlzd
   cntlzd :: repr GPR -> repr GPR

   -- | Counts the number of leading zeros of the 32-bit value in a source general-purpose register (GPR) and stores the result in a GPR.
   -- | Function: cntlzw
   cntlzw :: repr GPR -> repr GPR


-- || VECTOR FUNCTIONS || --

{-
Vector Select Instructions
-}

   {-
       Vector select (VSEL)

       For each bit in the third operand that contains a zero, the corresponding bit from the second
       operand is placed in the corresponding bit of the output register. For each bit in the third
       operand that contains a one, the corresponding bit from the first operand is placed in the
       corresponding bit of the output register.
   -}
   vsel :: repr VR -> repr VR -> repr VR -> repr VR
   xxsel :: repr VR -> repr VR -> repr VR -> repr VR

{-
Vector Permute Instructions
-}

   {-
       Vector Permute (VPERM)

      The Vector Permute instruction allows any byte in two source VSRs to be copied to any byte in the target VSR. The
      bytes in a third source VSR specify from which byte in the first two source VSRs the corresponding target byte is to
      be copied. The contents of the third source VSR are sometimes referred to as the “permute control vector”.
   -}
   vperm :: repr VR -> repr VR -> repr VR -> repr VR

   {-
       Vector Permute Right-indexed (VPERMR)

      The Vector Permute instruction allows any byte in two source VSRs to be copied to any byte in the target VSR. The
      bytes in a third source VSR specify from which byte in the first two source VSRs the corresponding target byte is to
      be copied. The contents of the third source VSR are sometimes referred to as the “permute control vector”.
   -}
   vpermr :: repr VR -> repr VR -> repr VR -> repr VR

{-
Vector Splat Instructions
-}

   {-
       Vector Splat Byte VX-form

       The contents of every byte is replaced by the byte indexed by the immediate argument.
   -}
   vspltb :: repr VR -> Integer -> repr VR

{-
Vector Merge Instructions
-}

   {-
       Vector Merge High Byte (VMRGHB)

       Merges the first half of two vectors interpreting each as a list of a bytes.
   -}
   vmrghb :: repr VR -> repr VR -> repr VR

   {-
       Vector Merge Low Byte (VMRGLB)

       Merges the second half of two vectors interpreting each as a list of a bytes.
   -}
   vmrglb :: repr VR -> repr VR -> repr VR

   {-
       Vector Merge High Halfword (VMRGHH)

       Merges the first half of two vectors interpreting each as a list of a halfwords (shorts).
   -}
   vmrghh :: repr VR -> repr VR -> repr VR

   {-
       Vector Merge Low Halfword (VMRGLH)

       Merges the second half of two vectors interpreting each as a list of a halfwords (shorts).
   -}
   vmrglh :: repr VR -> repr VR -> repr VR

   {-
       Vector Merge High Word (VMRGHW)

       Merges the first half of two vectors interpreting each as a list of a words.
   -}
   vmrghw :: repr VR -> repr VR -> repr VR

   {-
       Vector Merge Low Word (VMRGLW)

       Merges the second half of two vectors interpreting each as a list of a words.
   -}
   vmrglw :: repr VR -> repr VR -> repr VR

   {-
       Vector Merge Even Word (VMRGEW)

       Merges two vectors interpreting each as a list of a words such that VA[0] -> VT[0], VB[0] -> VT[1], VA[2] -> VT[2], VB[2] -> VT[3].
   -}
   vmrgew :: repr VR -> repr VR -> repr VR

   {-
       Vector Merge Odd Word (VMRGOW)

       Merges two vectors interpreting each as a list of a words such that VA[1] -> VT[0], VB[1] -> VT[1], VA[3] -> VT[2], VB[3] -> VT[3].
   -}
   vmrgow :: repr VR -> repr VR -> repr VR

   {-|
      Vector count leading zeros

      For each element in the second operand, the number of leftmost zeros are counted and placed in
      the corresponding element of the first operand

      The bits of each element in the second operand are scanned left to right for the leftmost one bit.
      A binary integer designating the bit position of the leftmost one bit, or the number of
      bits in the element if there is no one bit, is placed in the corresponding element of the
      first operand.

      The first input determines the element size:

         * 0      -> Byte
         * 1      -> Halfword
         * 2      -> Word
         * 3      -> Doubleword
         * 4-15 -> Reserved
   -}


{-
Vector Count Leading Zeros Instructions
-}

    {-
       Vector Count Leading Zeros Byte (VCLZB)

       Counts the leading zeros of a vector interpreted as a list of bytes.
   -}
   vclzb :: repr VR -> repr VR

   {-
       Vector Count Leading Zeros Halfword (VCLZH)

       Counts the leading zeros of a vector interpreted as a list of halfwords.
   -}
   vclzh :: repr VR -> repr VR

   {-
       Vector Count Leading Zeros Byte (VCLZW)

       Counts the leading zeros of a vector interpreted as a list of words.
   -}
   vclzw :: repr VR -> repr VR

   {-
       Vector Count Leading Zeros Byte (VCLZD)

       Counts the leading zeros of a vector interpreted as a list of double words.
   -}
   vclzd :: repr VR -> repr VR

   {-
       Vector Count Leading Zeros Byte (VCLZDM)

       Counts the leading zeros of a vector ra interpreted as a list of double words under a bit mask of rb.
   -}
   vclzdm :: repr VR -> repr VR -> repr VR


{-
Vector Floating Point Arithmetic Instructions
-}

    {-
       Floating Multiply-Add Double Precision (xvadddp)

       Takes the product of two floating point operands then adds another floating point to the result.
   -}
   xvmaddmdp :: repr VR -> repr VR -> repr VR -> repr VR

    {-
       Floating Multiply-Subtract Double precision (xvsubdp)

       Takes the product of two floating point operands then subtracts the result with another floating point.
   -}
   xvmsubmdp :: repr VR -> repr VR -> repr VR -> repr VR

   {-
       Floating Negative Multiply-Add Double precision (xvnmaddmsp)

       Multiplies 2 floating point operands and adds another floating point to the intermediate result. The result
       is then negated.
   -}
   xvnmaddmdp :: repr VR -> repr VR -> repr VR -> repr VR

   {-
       Floating Negative Multiply-Add Double precision (xvnmsubmsp)

       Multiplies 2 floating point operands and subtracts another floating point to the intermediate result. The result
       is then negated.
   -}
   xvnmsubmdp :: repr VR -> repr VR -> repr VR -> repr VR

    {-
       Vector Logical AND VX-form (vand)

       Returns the bitwise AND of the two inputs.
   -}
   vand                :: repr VR -> repr VR -> repr VR   -- VN
   xxland                :: repr VR -> repr VR -> repr VR   -- VN

   {-
       Vector Logical OR VX-form (vor)

       Returns the bitwise OR of the two inputs.
   -}
   vor :: repr VR -> repr VR -> repr VR   -- VO

   {-
       Vector Logical OR VSX-form (xxlor)

       Returns the bitwise OR of the two inputs.
   -}
   xxlor :: repr VR -> repr VR -> repr VR   -- VO

   {-
       Vector Logical OR with Complement VX-form (vorc)

       Returns the bitwise OR of the first input and the complement of the second input.
   -}
   vorc :: repr VR -> repr VR -> repr VR   -- VNO

   {-
       Vector Logical XOR VX-form (vxor)

       Returns the bitwise XOR of the two inputs.
   -}
   vxor :: repr VR -> repr VR -> repr VR   -- VX

   {-
       Vector Logical XOR with Complement VX-form (vxorc)

       Returns the bitwise XOR of the first input and the complement of the second input.
   -}
   vxorc :: repr VR -> repr VR -> repr VR

   {-
       Vector Logical AND with Complement VX-form (vandc)

       Returns the bitwise AND of the first input and the complement of the second input.
   -}
   vandc               :: repr VR -> repr VR -> repr VR   -- VNC

   {- |
       Vector floating-point addition (VFA)

       The floating-point elements of the first vector are added to the floating-point elements
       of the second vector.
   -}
   xvadddp                  :: repr VR -> repr VR -> repr VR

   {- |
       Vector floating-point subtract (VFS)

       The floating-point elements of the second vector are subtracted from the floating-point elements
       of the first vector.
   -}
   xvsubdp                  :: repr VR -> repr VR -> repr VR

   {- |
       Vector floating-point multiply (VFM)

       The floating-point elements of the first vector are multiplied by the floating-point elements
       of the second vector.
   -}
   xvmuldp                  :: repr VR -> repr VR -> repr VR


    {-
        VSX Vector Absolute Single-Precision XX2-form

        Takes a single precision vector and gets the absolute value of it.
    -}
--    xvabssp :: repr VR -> repr VR

    {-
       Floating Add Single Prcision (xvaddsp)

       Takes a floating point then adds another floating point to it.
   -}
   xvaddsp :: repr VR -> repr VR -> repr VR

   {-
       Floating Subtract Single Prcision (xvsubsp)

       Takes a floating point then subtracts another floating point from it.
   -}
   xvsubsp :: repr VR -> repr VR -> repr VR

   {-
       Floating Multiply Single Prcision (xvmulsp)

       Takes a floating point then multiplies another floating point with it.
   -}
   xvmulsp :: repr VR -> repr VR -> repr VR

    {-
       Floating Multiply-Add Single Prcision (xvadddp)

       Takes the product of two floating point operands then adds another floating point to the result.
   -}
   xvmaddmsp :: repr VR -> repr VR -> repr VR -> repr VR

    {-
       Floating Multiply-Add Single Prcision (xvnmaddmsp)

       Takes the negative product of two floating point operands then adds another floating point to the result.
   -}
   xvnmaddmsp :: repr VR -> repr VR -> repr VR -> repr VR

    {-
       Floating Multiply-Sub Single Prcision (xvmsubmsp)

       Takes the product of two floating point operands then subtracts the result with another floating point.
   -}
   xvmsubmsp :: repr VR -> repr VR -> repr VR -> repr VR

    {-
       Floating Multiply-Sub Single Prcision (xvnmsubmsp)

       Takes the negative product of two floating point operands then subtracts the result with another floating point.
   -}
   xvnmsubmsp :: repr VR -> repr VR -> repr VR -> repr VR


{- Functions from Old PowerInterp.lhs-}

    {- New functions to replace old ones -}

   xvcvdpsp :: {-forall repr. PowerISA repr =>-} repr VR -> repr VR

   vcfsx :: repr VR -> Integer -> repr VR
   vcfux :: repr VR -> Integer -> repr VR

   vctsxs :: repr VR -> Integer -> repr VR
   vctuxs :: repr VR -> Integer -> repr VR

   vrlwmi :: repr VR -> repr VR -> repr VR -> repr VR

    -- TODO: Overwrites first register
   vrlqmi :: repr VR -> repr VR -> repr VR -> repr VR
   vrlw :: repr VR -> repr VR -> repr VR
   vrlq :: repr VR -> repr VR -> repr VR

   vcmpgtub :: repr VR -> repr VR -> repr VR
   vcmpequw :: repr VR -> repr VR -> repr VR
   vcmpgtfp :: repr VR -> repr VR -> repr VR
   xvcmpgtsp :: repr VR -> repr VR -> repr VR
   vcmpgefp :: repr VR -> repr VR -> repr VR
   vcmpeqfp :: repr VR -> repr VR -> repr VR
   vcmpbfp :: repr VR -> repr VR -> repr VR

   vaddsws :: repr VR -> repr VR -> repr VR
   vadduwm :: repr VR -> repr VR -> repr VR
   vsubuwm :: repr VR -> repr VR -> repr VR

   vmodsw :: repr VR -> repr VR -> repr VR

   vnegw :: repr VR -> repr VR

   vmulosh :: repr VR -> repr VR -> repr VR

   vmuleuh :: repr VR -> repr VR -> repr VR

   -- TODO do we need this instruction?
   -- vmulhsw :: repr VR -> repr VR -> repr VR

-- TODO: check sizes of immediates in Power instructions
   s7, s10, s10to16 :: Integer -> Integer

   -- sign extension
--    sExt :: Integer -> Integer -> Integer -> Integer

   -- vector shift left
   vslw :: repr VR -> repr VR -> repr VR
   -- vector shift right
   vsrw :: repr VR -> repr VR -> repr VR
   -- vector shift right algebraic (sign extended)
   vsraw :: repr VR -> repr VR -> repr VR
   vslq :: repr VR -> repr VR -> repr VR

   ceqi :: repr VR -> Integer -> repr VR

--    divShiftMA :: PowerISA repr => Integer -> Integer -> Integer -> Integer -> repr VR -> repr VR

   --vcmpeqfp :: repr VR -> repr VR -> repr VR
--    lowhs :: repr VR -> [Word32]
--    lowhu :: repr VR -> [Word32]

   {- try to let recip work for inf and 0 on 23/06/2023-}
   --vcmpbfp :: repr VR -> repr VR -> repr VR
 {-
    Vector Reciprocal Estimate Floating-Point VX-form
    recipSqrt14bit
 -}
   vrefp :: repr VR -> repr VR
{-
    Vector Reciprocal Square Root Estimate Floating-Point VX-form
    estimate recip of sqrt
-}
   vrsqrtefp :: repr VR -> repr VR

{-
   Vector Insert Word from GPR using immediate-specified index VX-form
   insert [32:63] of gpr into vr using index
-}
   vinsw :: repr VR -> repr GPR -> Int -> repr VR
   tupleWord64ToListInt :: (Word64, Word64) -> [Int]
