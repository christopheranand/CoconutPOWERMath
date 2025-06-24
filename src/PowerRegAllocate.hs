{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
-- |
-- Module      :  PowerRegAllocate
-- Copyright   :  (c) OCA 2024
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for performing register allocation for
-- POWER 10 hardware

module PowerRegAllocate where

import Coconut.BaseTypes
import Coconut.Graph.CodeGraph
import Coconut.Graph.ControlFlow
import Coconut.Graph.DataFlow
import Coconut.RegisterAllocator
import Coconut.HashedSchedule
import Coconut.Schedule

import Data.ByteString.Char8 (ByteString)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict    as Map
import Data.Either (lefts,rights)
import qualified Data.List as List
import Data.Maybe (fromJust,fromMaybe)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.Graph.Inductive.Query.Dominators (dom)
import Data.Graph.Inductive.PatriciaTree (Gr)

import System.Random

import ISA.PowerHardware
import ISA.MetaData

regAllocatePower ::
  Schedule
  -> CodeGraph POWER
  -> [(String,ByteString)]
  -> Either ([(RegType POWER,Node,RegMap POWER)],LivenessSets)
            (RegMap POWER,SpillMap,ScheduledGraph POWER)
regAllocatePower schedule codeGraph inputRegs =
  let
    cfGraph :: ControlFlowGraph POWER
    cfGraph = cgControlFlow codeGraph

    cfIn :: CFNode
    cfIn = case matchCFNode (cgInput codeGraph) cfGraph of
             Just cfNode -> cfNode
             Nothing -> error "CodeGraph missing cgIn in ControlFlowGraph"

    inpNodes :: [(String,Node)]
    inpNodes = case cfIn of
                 CFNode inps -> inps
                 _ -> error "cgInput is not a CFNode"

    -- nodes (already remapped to their parent coalesced node) that have fixed input registers
    fixedNodes :: [(Node,ByteString)]
    fixedNodes = [(\ (n, r)
                    -> case IM.lookup n regCoalesceMap of
                                Just n' -> (n', r)
                                Nothing -> (n, r))
                  (n, r) |
                  (s0, r) <- inputRegs, (s1, n) <- inpNodes, s0 == s1]

    scheduledGraph :: ScheduledGraph POWER
    scheduledGraph = buildScheduledGraph schedule codeGraph

    -- regCoalesceMap maps nodes to their parent nodes (i.e., the node
    -- represented in the interferenceGraph)
    regCoalesceMap :: IntMap Node
    interferenceGraph :: InterferenceGraph POWER
    liveSets :: LivenessSets
    (regCoalesceMap,liveSets,interferenceGraph) = genInterferenceGraph codeGraph scheduledGraph

    nodeIsRegType :: RegType POWER -> RegNode POWER -> Bool
    nodeIsRegType regType (RegNode resType) = case resType of
                                                (RegisterRes regType') -> regType == regType'
                                                (MemoryRes _) -> regType == memRegType
                                                _ -> False

    nodeIsSpill :: RegNode h -> Bool
    nodeIsSpill (RegNode resType) = case resType of
                                      SpillRes -> True
                                      _ -> False

    spillNodes = nodes $ labfilter nodeIsSpill interferenceGraph
    spillMap0 = Map.fromList $ zip spillNodes [0..]
                 `debug` ("\nspillNodes: " ++ show spillNodes ++ "\n")
    spillMap = Map.union spillMap0 $
              Map.fromList $
              concatMap (\(n0,n) -> case Map.lookup n spillMap0 of
                                       Just offset -> [(n0, offset)]
                                       Nothing -> []
                      ) $ IM.toList regCoalesceMap

    interGraphsByReg :: [(RegType POWER,InterferenceGraph POWER)]
    interGraphsByReg = map (\regType -> (regType
                                        ,labfilter (nodeIsRegType regType) interferenceGraph))
                           [GPR,VR] -- (allRegTypes @POWER)

    (_,interGraphGPR) = head interGraphsByReg
    (_,interGraphVR) = interGraphsByReg !! 1

    gprColors = chaitinBriggsAllocate (regColors GPR) (GPR,interGraphGPR) fixedNodes

    nonVSXColors = drop 32 (regColors VR)
    -- find all regs used by non-vsx instrs by traversing ScheduledGraph
    nonVSXRegisters = List.nub
      $ map ((\n -> fromMaybe n (IM.lookup n regCoalesceMap)) . fst)
      $ concatMap
      (\(_,sNode) ->
          if isNonVSXInstruction codeGraph (snInstructionNode sNode)
             then snUses sNode ++ snDefs sNode
             else []
                ) $ labNodes scheduledGraph
    interGraphNonVSX = nfilter (`elem` nonVSXRegisters) interGraphVR
    eRegMap0 = chaitinBriggsAllocate nonVSXColors (VR,interGraphNonVSX) fixedNodes
    nonVSXColorsT = map (\c -> (VR,c)) nonVSXColors
    isNonVSXReg n = let n' = fromMaybe n (IM.lookup n regCoalesceMap)
                    in n' `elem` nonVSXRegisters
    (eRegMap1,scheduledGraph1) = case eRegMap0 of
                                Right regMap0 ->
                                        case allocateGlobalConstants0 codeGraph
                                                                        schedule
                                                                        regMap0
                                                                        scheduledGraph
                                                                        isNonVSXReg
                                                                        nonVSXColorsT
                                        of
                                        (r,s) -> (Right r,s)
                                leftVal -> (leftVal,scheduledGraph)
    allocatedGlobalNonVSXColors =
      case (eRegMap1,eRegMap0) of
           (Right regMap1,Right regMap0) ->
             List.nub
             $ map snd
             $ Map.elems
             $ Map.differenceWith (\a1 a0 -> if a1 /= a0 then Just a1 else Nothing) regMap1 regMap0
           _ -> []
    vsxColors = filter (`notElem` allocatedGlobalNonVSXColors) $ regColors VR
                `debug` ("allocatedGlobalNonVSXColors: " ++ show allocatedGlobalNonVSXColors)

    eRegMap2 = case eRegMap1 of
      Right regMap1 -> chaitinBriggsAllocate0 vsxColors (VR,interGraphVR) fixedNodes regMap1
      leftVal -> leftVal

    vrColors = eRegMap2

    coloringResults :: [Either (RegType POWER,Node,RegMap POWER) (RegMap POWER)]
    coloringResults = [gprColors,vrColors]
                       -- map (\(r,g) ->
                       --       chaitinBriggsAllocate (regColors r) (r,g) fixedNodes) interGraphsByReg

    failedAllocations :: [(RegType POWER,Node,RegMap POWER)]
    failedAllocations = lefts coloringResults

    combinedRegMaps :: RegMap POWER
    combinedRegMaps = foldl Map.union Map.empty $ rights coloringResults

    missingRegMaps = IM.map (\n -> case Map.lookup n combinedRegMaps of
                                    Just n' -> n'
                                    Nothing -> error $ "failed to lookup node: " ++ show n
                                               ++ "\nregCoalesceMap: " ++ show regCoalesceMap
                                               ++ "\ncombinedRegMaps: " ++ show combinedRegMaps
                            )
                     $ IM.filter (`notElem` spillNodes)regCoalesceMap

    allRegMaps = Map.fromList (IM.toList missingRegMaps) `Map.union` combinedRegMaps
  in case failedAllocations of
       [] ->
         let
           (regMapC,schedGraphC) =
             -- (allRegMaps,scheduledGraph1)
             -- allocateGlobalConstants codeGraph schedule allRegMaps scheduledGraph1

             allocateGlobalConstants0 codeGraph
                                      schedule
                                      allRegMaps
                                      scheduledGraph1
                                      (not . isNonVSXReg)
                                      (map (GPR,) (regColors GPR) ++ map (VR,) (regColors VR))
         in Right (regMapC,spillMap,schedGraphC)
            `debug` ("\n\neRegMap0:\n" ++ show eRegMap0
                    ++"\n\neRegMap1:\n" ++ show eRegMap1
                    ++"\n\neRegMap2:\n" ++ show eRegMap2
                    ++"\n\nDiff eRegMap0 eRegMap1: " ++
                           case (eRegMap0,eRegMap1) of
                             (Right regMap0,Right regMap1) ->
                               show (Map.differenceWith (\a1 a0 -> if a1 /= a0 then Just a1 else Nothing) regMap1 regMap0)
                             _ -> "")
       ns -> Left (ns,liveSets)

nonVSXInstructions = map fst $ filter (mdIsNonVSX . snd) $ Map.toList $ mdMap powerMetaData

isNonVSXInstruction cg node =
  case matchNodeInCG cg node of
      (CGDataFlowNode (_,dfNode)) ->
        case dfNode of
          (InstructionNode instr) ->
            case instr of
              Instruction _ instrName _ -> instrName `elem` nonVSXInstructions
              Move instrName _ -> instrName `elem` nonVSXInstructions
              _ -> False
          _ -> False
      _ -> False

-- | Performs scheduling then register allocation. If register allocation
-- fails, add a spill to the CodeGraph and reschedule. Repeat until register
-- alllocation suceeds
scheduleAndRegAllocatePower ::
  CodeGraph POWER
  -> Int
  -> MDMap POWER
  -> [HashedPenalty POWER]
  -> [HashedConstraint POWER]
  -> Maybe StdGen
  -> Bool
  -> Solver
  -> Int
  -> [(String,ByteString)]
  -> IO (CodeGraph POWER,Schedule,RegMap POWER,SpillMap,ScheduledGraph POWER,Double,StdGen)
scheduleAndRegAllocatePower cg unroll metaData penalties constraints stdgen0 reuse solver numStages fixedRegs =
  do (cg',sched,ii,stdgen,hashedData) <- genHashedSchedule cg unroll metaData penalties constraints stdgen0 reuse solver numStages
     let eRegMap = regAllocatePower sched cg' fixedRegs
           -- regAllocateCG sched cg' fixedRegs
     case eRegMap of
       Right (rMap,sMap,sGraph) -> do
--         putStrLn $ "\nSpillMap after regAllocate: " ++ show sMap ++ "\n"
         return (cg',sched,rMap,sMap,sGraph,ii,stdgen)
       Left (failedNodes,liveSets) ->
         error $ "reg allocation failed: " ++ show (failedNodes,liveSets)
         -- let
         --  spills0 = failedSpillCandidates hashedData cg' failedNodes liveSets
         --  spillsD = defaultSpillCandidates hashedData
         --  -- failedSpillCandidates might not return a spill for every failed regType
         --  -- in this case, use defaultSpillCandidate instead
         --  spills = map (\(rt,mN) -> case mN of
         --                   Just n -> (rt,n)
         --                            `debug` ("failedSpill selected: " ++ show n)
         --                   Nothing -> case filter (\(rt',_) -> rt == rt') spillsD of
         --                                (n:_) -> n
         --                                         `debug` ("defaultSpill selected: " ++ show n)
         --                                [] -> error $ "regType missing from default spills: "
         --                                      ++ show (rt,spillsD)
         --                   ) spills0
         --  -- only keep spills of register types that failed
         --  cgN = addSpill cg spills
         -- in do putStrLn $ "Reg Alloc failed on nodes: " ++ show failedNodes
         --                ++ "\nadding spills: " ++ show spills
         --       scheduleAndRegAllocatePower cgN unroll metaData penalties constraints stdgen0 reuse solver numStages fixedRegs
