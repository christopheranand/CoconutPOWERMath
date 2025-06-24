{-# LANGUAGE TypeApplications, ScopedTypeVariables,RankNTypes, OverloadedStrings #-}
module Main (main) where

import MathLibPower

import ISA.PowerISA
import ISA.PowerInterp ()
import ISA.PowerHardware
import ISA.MetaData (powerMetaData)

import Activation
import Exp
import MathUtils

import Coconut.BaseTypes
import Coconut.Graph.Dot

import qualified Data.ByteString.Char8 as BS

-- Number of iterations to run when doing stochastic code generation
numIter = 300

defaultNStages :: Int
defaultNStages = 2

-----------------------------------------------------------------------
-- * Activiation Function CodeGraphs

geluOneWrap :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
geluOneWrap = wrap44Q ("geluOne",qMap geluOne,4)

iterateGeluOneCodeGen = iterateCodeGen geluOneWrap defaultPenalties defaultConstraints defaultNStages numIter

-- geluTwoWrap :: forall h. (Hardware h) =>
--   (String, CodeGraph h, Int)
-- geluTwoWrap = wrap44Q ("geluTwo",qMap geluTwo,4)

geluThreeWrap :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
geluThreeWrap = wrap44Q ("geluThree",qMap geluThree,4)

siluWrap :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
siluWrap = wrap44Q ("silu",qMap silu,4)

sigmoidWrap :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
sigmoidWrap = wrap44Q ("sigmoid",qMap sigmoid,4)

sigmoidWrap8 :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
sigmoidWrap8 = wrap88Q ("sigmoid",eightMap sigmoid,8)

iterateSigmoidCodeGen = iterateCodeGen sigmoidWrap defaultPenalties defaultConstraints defaultNStages numIter

softplusWrap :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
softplusWrap = wrap44Q ("softplus",qMap softplus,4)

testAddWrap :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
testAddWrap = wrap44Q ("testadd",qMap testAdd,4)
testAdd :: PowerISA repr => repr VR -> repr VR
testAdd v0 =
  let
    v1 = unfloats4 1.0
    -- v1 = undoubles2 1.0
  in xvaddsp v0 v1

expWrap :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
expWrap = wrap44Q ("exp",qMap expSP,4)

expWrap8 :: forall h. (Hardware h) =>
  (String, CodeGraph h, Int)
expWrap8 = wrap88Q ("vsexp",eightMap expSP,8)

iterateExpCodeGen = iterateCodeGen expWrap defaultPenalties defaultConstraints defaultNStages numIter

-----------------------------------------------------------------------
-- * Main

main :: IO ()
main =
  let
    cgWrapped@(name,cg,unroll) = expWrap8
                                 -- sigmoidWrap8
                                 -- expWrap
  in do
    -- dotCompilationWithOpts @POWER dotCompilationPowerOpts cg
    -- mathLibPowerCodeGen cgWrapped defaultPenalties0 defaultConstraints Nothing False
    -- mathLibPowerDotGraph cgWrapped
    -- mathLibPowerScheduleGraphWithReg cgWrapped defaultPenalties0 defaultConstraints Nothing True

    e <- mathLibPowerCodeGenAndSimulate cgWrapped defaultPenalties0 defaultConstraints defaultNStages Nothing False
    case e of
      Left e -> error $ "Encountered SimError: " ++ show e
      Right p -> print p

    -- e <- mathLibPowerSimulate cgWrapped defaultPenalties0 defaultConstraints defaultNStages Nothing False
    -- case e of
    --   Left e -> error $ "Encountered SimError: " ++ show e
    --   Right p -> print p

    -- iterateSigmoidCodeGen
