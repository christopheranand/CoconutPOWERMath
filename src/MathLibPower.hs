{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, TypeApplications, DataKinds, RankNTypes, OverloadedStrings #-}

module MathLibPower where

--import CoconutHypergraph.NodeName                   ( NodeName )
import PrelExts hiding (fromRight)

import Control.Monad (forM_)

import Coconut.BaseTypes
import Coconut.CodeGen
import Coconut.Core.ControlISA
import Coconut.Utils.CGWrappers
import Coconut.Graph.CodeGraph
import Coconut.Core.CoreISA

import ISA.PowerInterp ()
import ISA.PowerISA
import ISA.PowerCodeGraph
import ISA.PowerHardware (POWER)
import Coconut.Graph.Dot
import Coconut.Schedule
import Coconut.HashedSchedule
import Coconut.Graph.DataFlow (instructionConsumersL,instructionConsumers)
import ISA.PowerHardware
import ISA.MetaData (powerMetaData)
import Coconut.RegisterAllocator
import Coconut.Simulator
import Coconut.Rewrite
import Activation
import PowerRegAllocate
--import ZCodeGraphExamples ()

import MathUtils
import Sqrt

import SafePrelude
import GHC.TypeLits
import Data.Proxy
import Data.Maybe (fromJust)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import System.Random

import HashedExpression hiding (Node,VR)
import HashedExpression.Modeling.Typed

-- TODO remove me
import Data.Char (toUpper)
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Coconut.HashedSchedule (spillConstraints, {-finalInstrConstraint -})

eefErr = error "what is eef"
eef1 = 1 :: Int
eef2 = 2 :: Int

defaultSolver = Ipopt

defaultScale (n,dfNode) = 10000

defaultPenalties0 = [topBottomPenalty @POWER defaultScale,pushLoadConsumes,spillPenalty]
defaultPenalties = [topBottomPenalty @POWER defaultScale,pushLoadConsumes, stochasticPenalty 100.0,spillPenalty]
defaultConstraints = [varBounds @POWER
                     ,completionConstraints
                     ,subjectConstraints
                     ,overwriteConstraints
                     ,latConstraints
                     ,gprConstraints
                     ,spillConstraints
                     ,finalInstrConstraint
                 ]

pushLoadConsumes hashedData =
  let
    dataFlow = hdDataFlow hashedData
    dfGraph= dataFlowGraph dataFlow
    instructions = hdAllInstructions hashedData
    tVars = hdTVarMap hashedData

    -- find all lgMR nodes
    lgMRs = filter (\(n,dfNode) -> dfNodeHasName dfNode (=="lgMR")) instructions
    -- find all vlvgp's that consume a given node
    findVlvgpConsumer (n,dfNode) = filter (\(n,dfNode) -> dfNodeHasName dfNode (=="vlvgp"))
                                   $ instructionConsumersL dataFlow n
    -- make pairs of lgMRs and vlvgp consumers
    lgMR_vlvgps = concatMap (\(n0,dfNode0) -> map (\(n1,_) -> (n0,n1))
                                              $ findVlvgpConsumer (n0,dfNode0)) lgMRs
    scale = -10000
  in map (\(n0,n1) -> scale * (tVars Map.! n1 - tVars Map.! n0)) lgMR_vlvgps

-- FIXME switch this to penalty?
-- latConstraints :: (Hardware h)
--                => HashedData h
--                -> [ConstraintDecl]
latConstraints hashedData = let
  dfGraph = hdDataFlow hashedData
  instructions = hdAllInstructions hashedData
  metaData = hdMetaData hashedData
  tVar = hdTVarMap hashedData

  -- TODO move me into Hardware typeclass and do a more thorough job identifying stores/loads
  -- NOTE not including spills in here will make problem infeasible?
  notLoadStore name = case name of
                        "lgMR" -> False
                        ('s':'p':'i':'l':'l':_) -> False
                        ('d':'e':'s':'p':'i':'l':'l':_) -> False
                        ('u':'n':_) -> False
                        _ -> True
  vrs    = filterByName notLoadStore instructions
    -- filterByName (not . (`elem` ["lgMR","vlgv0","stv0MR"])) instructions
  -- lat1 :: [(Node,Node,Int)]
  lat1 = map (\(i,j) -> (i,j,latOrErr metaData dfGraph i)) $ mkDependencies dfGraph vrs
  completionDepth :: Double
  completionDepth = 1
  deepen lats = concatMap (\(i,j,depth) ->
                             if isStore dfGraph j
                             then []
                             else [(i,k,depth + latOrErr metaData dfGraph j) | k <- instructionConsumers dfGraph j]) lats

  latencyPairs = L.nubBy ( \(i,j,_) (l,m,_) -> i==l && j==m )
                 $ L.reverse
                 $ L.sort
                 $ filter (\(_,_,depth) -> depth > completionDepth)
                 -- $ deepen $ deepen $ deepen lat1
                 $ deepen lat1

  in (map (\(i,j,depth) ->
            (tVar Map.! j - tVar Map.! i) .>=
              (depth - completionDepth))
      latencyPairs)
     -- `debug` ("\nlatencyPairs: " ++ show latencyPairs ++ "\n")

defaultStages = 2

defaultFixedRegs = [("size","5"),("mrIn","4"),("mrOut","3")]

dotCompilationPowerOpts =
  DotCompilationOpts
    { dfgOpts = NoTimeNodes
    , graphCompileOpts = NoCompile
    , showConstantOpts = NoShowConstants
    }

dotCompilationPowerOpts2 =
  DotCompilationOpts
    { dfgOpts = NoTimeNodes
    , graphCompileOpts = NoCompile
    , showConstantOpts = ShowConstants
    }

-----------------------------------------------------------------------
-- * Simulation

mathLibPowerSimulate :: (String, CodeGraph POWER, Int)
                 -> [HashedPenalty POWER]
                 -> [HashedConstraint POWER]
                 -> Maybe StdGen
                 -> Bool
                 -> IO (Either (SimError POWER) (HardwareST POWER))
mathLibPowerSimulate wrappedCG penalties constraints stdgen reuse = do
  (cg,sched,regMap,spillMap,schedGraph,ii,_) <- scheduleAndRegAllocatePower moduloCG unroll powerMetaData penalties constraints stdgen reuse defaultSolver defaultStages defaultFixedRegs
  initSimulator ("asm/"++name++".dbxin" ) schedGraph cg (regMap,spillMap) [mrIn, mrOut] (initGPR, [])
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0
    -- mrIn  = ("mrIn", "4", map undoubles $ zipWith (\x y -> [x,y]) [0,2..6] [1,3..7])
    mrIn  = ("mrIn", "4", map undoubles $ zipWith (\x y -> [x,y]) [0,2..14] [1,3..15])
    -- mrOut = ("mrOut", "3", map undoubles2 $ replicate 8 99.9)
    mrOut = ("mrOut", "3", map undoubles2 $ replicate 16 99.9)
    -- initGPR = [("5", unintegerG (8*8))]
    initGPR = [("5", unintegerG (8*8*2))]

------------------------------------------------------------------
-- * Code generation

mathLibPowerCodeGen :: (String, CodeGraph POWER, Int)
                -> [HashedPenalty POWER]
                -> [HashedConstraint POWER]
                -> Maybe StdGen
                -> Bool
                -> IO ()
mathLibPowerCodeGen wrappedCG penalties constraints stdgen reuse = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocatePower moduloCG unroll powerMetaData penalties constraints stdgen reuse defaultSolver defaultStages defaultFixedRegs
  let (code,tables) = codeGenCG cg (regMap,spillMap,schedGraph) powerMetaData
      allCode = mathLibPowerHeader (name,stdgen') doublePrecision ++ code ++ mathLibPowerReturn name ++ tables ++ mathLibPowerEnd
              ++ ["# .bss section","# dwarf sections","# end dwarf sections"]
  BS.writeFile ("asm/"++name++".s"  )$ BS.unlines allCode
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0
    doublePrecision = False

mathLibPowerCodeGenAndSimulate :: (String, CodeGraph POWER, Int)
                -> [HashedPenalty POWER]
                -> [HashedConstraint POWER]
                -> Int
                -> Maybe StdGen
                -> Bool
                -> IO (Either (SimError POWER) (HardwareST POWER))
mathLibPowerCodeGenAndSimulate wrappedCG penalties constraints numStages stdgen reuse = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocatePower moduloCG unroll powerMetaData penalties constraints stdgen reuse defaultSolver numStages defaultFixedRegs
  let (code,tables) = codeGenCG cg (regMap,spillMap,schedGraph) powerMetaData
      allCode = mathLibPowerHeader (name,stdgen') doublePrecision ++ code
              ++ mathLibPowerReturn name ++ tables ++ mathLibPowerEnd
  BS.writeFile ("asm/"++name++".s"  )$ BS.unlines allCode
  initSimulator ("asm/"++name++".dbxin" ) schedGraph cg (regMap,spillMap) [mrIn, mrOut] (initGPR, [])
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0
    doublePrecision = False
    mrIn  = ("mrIn", "4", map unfloats $ [[0,1,2,3]
                                         ,[4,5,6,7]
                                         ,[8,9,10,11]
                                         ,[12,13,14,15]
                                         -- ,[16,17,18,19]
                                         -- ,[20,21,22,23]
                                         -- ,[24,25,26,27]
                                         -- ,[28,29,30,31]
                                         ])
    -- mrOut = ("mrOut", "3", map undoubles2 $ replicate 16 99.9)
    mrOut = ("mrOut", "3", map undoubles2 $ replicate 8 99.9)
    -- initGPR = [("5", unintegerG (2*8*8))] -- size
    initGPR = [("5", unintegerG (8*8))] -- size

--------------------------------------------------------------------
-- * Dot Graph Compilation

-- | Generate dot graphs (defaults to "./dot" directory) for a given CodeGraph
-- (with @cgApplyRewrites@ applied)
mathLibPowerDotGraph :: (String, CodeGraph POWER, Int) -> IO ()
mathLibPowerDotGraph wrappedCG = do
  dotCompilation moduloCG
  where
    (name,moduloCG0,_ ) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0

-- | Generate dot graphs (defaults to "./dot" directory) for a given modulo
-- generated CodeGraph (i.e. the modulo codegraph return by @genHashedSchedule@)
mathLibPowerModuloDotGraph :: (String, CodeGraph POWER, Int)
                       -> [HashedPenalty POWER]
                       -> [HashedConstraint POWER]
                       -> Maybe StdGen
                       -> Bool
                       -> IO ()
mathLibPowerModuloDotGraph wrappedCG penalites constraints stdgen reuse = do
  (cg,sched,ii,_,_) <- genHashedSchedule moduloCG unroll powerMetaData penalites constraints stdgen reuse defaultSolver defaultStages
  dotCompilation cg
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0

-- | Generate dot graphs (defaults to "./dot" directory) for the loop body of a given modulo
-- generated CodeGraph (i.e. the modulo codegraph return by @genHashedSchedule@). Uses
-- dotCompileIIGraph
mathLibPowerModuloBodyDotGraph :: (String, CodeGraph POWER, Int)
                           -> [HashedPenalty POWER]
                           -> [HashedConstraint POWER]
                           -> Maybe StdGen
                           -> Bool
                           -> IO ()
mathLibPowerModuloBodyDotGraph wrappedCG penalites constraints stdgen reuse = do
  (cg,sched,ii,_,_) <- genHashedSchedule moduloCG unroll powerMetaData penalites constraints stdgen reuse defaultSolver defaultStages
  let dataFlows = cgDataFlows cg
      moduloBody = case filter (\(name,_) -> dropWhile (/='_') name == "_KERNEL") dataFlows of
                     [] -> error "no dataflowgraph with the suffix _KERNEL"
                     ((name,dfGraph):_) -> dfGraph
  dotCompileIIGraph ("dot/"++name++"_moduloII.dot") moduloBody sched powerMetaData ii
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0

-- | Generate a dot graph of the corresponding @ScheduledGraph@ generated from a
--  applying @genHashedSchedule@ to a given CodeGraph
mathLibPowerScheduleGraph :: (String, CodeGraph POWER, Int)
                      -> [HashedPenalty POWER]
                      -> [HashedConstraint POWER]
                      -> Maybe StdGen
                      -> Bool
                      -> IO ()
mathLibPowerScheduleGraph wrappedCG penalites constraints stdgen reuse = do
  (cg,sched,ii,_,_) <- genHashedSchedule moduloCG unroll powerMetaData penalites constraints stdgen reuse defaultSolver defaultStages
  putStrLn "generating dot graph"
  dotStringScheduleFormatted cg (buildScheduledGraph sched cg) sched "./dot"
  putStrLn "done"
  -- dotCompilationInterference (snd $ genInterferenceGraph cg sched) (fromRight Map.empty $ regAllocateCG sched cg [("size","3"),("mrIn","2"),("mrOut","1")]) "./dot"
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0

-- | Generate a dot graph of the corresponding @ScheduledGraph@ generated from a
--  applying @genHashedSchedule@ to a given CodeGraph
mathLibPowerScheduleGraphWithReg :: (String, CodeGraph POWER, Int)
                      -> [HashedPenalty POWER]
                      -> [HashedConstraint POWER]
                      -> Maybe StdGen
                      -> Bool
                      -> IO ()
mathLibPowerScheduleGraphWithReg wrappedCG penalties constraints stdgen reuse = do
  (cg,sched,regMap,spillMap,schedGraph,ii,_) <- scheduleAndRegAllocatePower moduloCG unroll powerMetaData penalties constraints stdgen reuse defaultSolver defaultStages defaultFixedRegs
  -- let (regMap,schedGraph) = fromRight (error "mathLibPowerScheduleGraphWithReg regAllocateCG failed")
  --                           $ regAllocateCG sched cg [("size","3"),("mrIn","2"),("mrOut","1")]
  putStrLn "generating dot graph"
  dotStringScheduleFormatted cg schedGraph sched "./dot"
  dotStringScheduleWithRegMapFormatted cg regMap schedGraph "./dot"
  putStrLn $ show sched
  putStrLn "done"
  -- dotCompilationInterference (snd $ genInterferenceGraph cg sched) (fromRight Map.empty $ regAllocateCG sched cg [("size","3"),("mrIn","2"),("mrOut","1")]) "./dot"
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0

-- | Print a modulo generated codegraph
mathLibPowerPrintModuloCG :: (String, CodeGraph POWER, Int)
                       -> [HashedPenalty POWER]
                       -> [HashedConstraint POWER]
                       -> Maybe StdGen
                       -> Bool
                       -> IO ()
mathLibPowerPrintModuloCG wrappedCG penalites constraints stdgen reuse = do
  (cg,sched,ii,_,_) <- genHashedSchedule moduloCG unroll powerMetaData penalites constraints stdgen reuse defaultSolver defaultStages
  putStrLn "\nModulo CodeGraph\n"
  putStrLn $ unlines $ prettyCodeGraph cg
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0




------------------------------------------------------------------
-- * Utilities
-- TODO look at this, why is moduloVLoad/Store carrying around two memory regions
-- TODO we need to load to account for little endian big endian
wrap44Q :: forall h. (Hardware h)
  => (String, (Graph h VR, Graph h VR, Graph h VR, Graph h VR) -> (Graph h VR, Graph h VR, Graph h VR, Graph h VR), Int)
  -> (String, CodeGraph h, Int)
wrap44Q (name,f,eef) =
  let
    wrapped :: (Graph h GPR,Graph h GPR,Graph h MR, Graph h MR)
      -> (Graph h GPR, Graph h GPR,Graph h MR, Graph h MR)
    wrapped (gCnt,gSize,inMR,outMR) =
      let (x1,in1) = moduloVLoad inMR (incMR inMR gCnt) 0
          (x2,in2) = moduloVLoad in1 (incMR in1 gCnt) 16
          (x3,in3) = moduloVLoad in2 (incMR in2 gCnt) 32
          (x4,inMR') = moduloVLoad in3 (incMR in3 gCnt) 48
          (u1,u2,u3,u4) = f (x1,x2,x3,x4)
          out1 = moduloVStore outMR (incMR outMR gCnt) 0 u1
          out2 = moduloVStore out1 (incMR out1 gCnt) 16 u2
          out3 = moduloVStore out2 (incMR out2 gCnt) 32 u3
          outMR' = moduloVStore out3 (incMR out3 gCnt) 48 u4
      in (gCnt,gSize,inMR',outMR')
  in (name,createCG (genBlock (moduloBlock name
                               ["cnt","size","mrIn","mrOut"]
                               ["cnt","size","mrIn","mrOut"] wrapped)),4)

wrap88Q :: forall h. (Hardware h)
  => (String
     ,(Graph h VR, Graph h VR, Graph h VR, Graph h VR,Graph h VR, Graph h VR, Graph h VR, Graph h VR)
       -> (Graph h VR, Graph h VR, Graph h VR, Graph h VR,Graph h VR, Graph h VR, Graph h VR, Graph h VR)
     , Int)
  -> (String, CodeGraph h, Int)
wrap88Q (name,f,eef) =
  let
    wrapped :: (Graph h GPR,Graph h GPR,Graph h MR, Graph h MR)
      -> (Graph h GPR, Graph h GPR,Graph h MR, Graph h MR)
    wrapped (gCnt,gSize,inMR,outMR) =
      let (x1,in1) = moduloVLoad inMR (incMR inMR gCnt) 0
          (x2,in2) = moduloVLoad in1 (incMR in1 gCnt) 16
          (x3,in3) = moduloVLoad in2 (incMR in2 gCnt) 32
          (x4,in4) = moduloVLoad in3 (incMR in3 gCnt) 48
          (x5,in5) = moduloVLoad in4 (incMR in4 gCnt) 64
          (x6,in6) = moduloVLoad in5 (incMR in5 gCnt) 80
          (x7,in7) = moduloVLoad in6 (incMR in6 gCnt) 96
          (x8,inMR') = moduloVLoad in7 (incMR in7 gCnt) 112
          (u1,u2,u3,u4,u5,u6,u7,u8) = f (x1,x2,x3,x4,x5,x6,x7,x8)
          out1 = moduloVStore outMR (incMR outMR gCnt) 0 u1
          out2 = moduloVStore out1 (incMR out1 gCnt) 16 u2
          out3 = moduloVStore out2 (incMR out2 gCnt) 32 u3
          out4 = moduloVStore out3 (incMR out3 gCnt) 48 u4
          out5 = moduloVStore out4 (incMR out4 gCnt) 64 u5
          out6 = moduloVStore out5 (incMR out5 gCnt) 80 u6
          out7 = moduloVStore out6 (incMR out6 gCnt) 96 u7
          outMR' = moduloVStore out7 (incMR out7 gCnt) 112 u8
      in (gCnt,gSize,inMR',outMR')
  in (name,createCG (genBlock (moduloBlock name
                               ["cnt","size","mrIn","mrOut"]
                               ["cnt","size","mrIn","mrOut"] wrapped)),8)

-- wrap44Q :: forall h. (Hardware h)
--   => (String, (Graph h VR, Graph h VR, Graph h VR, Graph h VR) -> (Graph h VR, Graph h VR, Graph h VR, Graph h VR), Int)
--   -> (String, CodeGraph h, Int)
-- wrap44Q (name,f,eef) =
--   let
--     wrapped :: (Graph h GPR,Graph h GPR,Graph h MR, Graph h MR,Graph h GPR,Graph h GPR)
--       -> (Graph h GPR, Graph h GPR,Graph h MR, Graph h MR,Graph h GPR,Graph h GPR)
--     wrapped (gCnt,gSize,inMR,outMR,fifo0,fifo1) =
--       let (x1,in1) = moduloVLoad inMR 0 gCnt
--           (x2,in2) = lvx in1 (addiG gCnt 16)
--           (x3,in3) = lvx in2 (addiG gCnt 32)
--           (x4,inMR') = lvx in3 (addiG gCnt 48)
--           (u1,u2,u3,u4) = f (x1,x2,x3,x4)
--           out1 = stvx outMR  gCnt u1
--           out2 = stvx out1   (addiG gCnt 16) u2
--           out3 = stvx out2   (addiG gCnt 32) u3
--           outMR' = stvx out3 (addiG gCnt 48) u4
--       in (gCnt,gSize,inMR',outMR',fifo0,fifo1)
--   in (name,createCG (genBlock (moduloBlock name
--                                ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
--                                ["cnt","size","mrIn","mrOut","fifo0","fifo1"] wrapped)),4)

wrapBasic :: forall h. (Hardware h)
  => (String, Graph h VR -> Graph h VR)  -> (String, CodeGraph h)
wrapBasic (name,f) =
  let
    preLabels = ["size", "mrIn", "mrOut"]
    allLabels = ["cnt","size","mrIn","mrOut"]
    outputLabelBranch = ["branchCond","cnt","size","mrIn","mrOut"]

    preCond :: (Graph h GPR, Graph h MR, Graph h MR)
      -> (Graph h GPR,Graph h GPR,Graph h MR, Graph h MR)
    preCond (gSize,inMR,outMR) =
      let
        gCnt = unintegerG 0
        fakeAdd = mulldG gCnt (unintegerG 1)
      in
        (fakeAdd,gSize,inMR,outMR)

    wrapped :: (Graph h GPR,Graph h GPR,Graph h MR, Graph h MR)
      -> (Graph h BRANCH, (Graph h GPR, Graph h GPR,Graph h MR, Graph h MR))
    wrapped (gCnt,gSize,inMR,outMR) =
      let (x1,inMR') = lvx inMR gCnt
          u1 = f x1
          outMR' = stvx outMR gCnt u1
          gCnt' = addiG gCnt 16
          -- TODO: cmp shold return repr CR, which we check as a branch condition
          loopCond = error "implement branch" -- cmp gSize gCnt' (unintegerG 0)
      in (loopCond, (gCnt',gSize,inMR',outMR'))

    postCond :: (Graph h GPR,Graph h GPR,Graph h MR, Graph h MR)
      -> (Graph h GPR,Graph h GPR,Graph h MR, Graph h MR)
    postCond (gCnt,gSize,inMR,outMR) =
      let gCnt' = addiG gCnt 16
      in (gCnt',gSize,inMR,outMR)

    whileLoopWrapper =
      doWhile
      (branchingBlock (name ++ "Loop") allLabels outputLabelBranch wrapped)
      (basicBlock (name ++ "Post") allLabels allLabels postCond)

    graphWrapper =
      compose
      (basicBlock (name ++ "Pre") preLabels allLabels preCond)
      whileLoopWrapper

  in (name,createCG (genBlock graphWrapper))


wrapNoLoop :: forall h. (Hardware h)
  => (String, Graph h VR -> Graph h VR)  -> (String, CodeGraph h)
wrapNoLoop (name,f) =
  let
    inpLabel = ["input"]
    resLabel = ["result"]
    block = basicBlock name inpLabel resLabel f
  in (name,createCG $ genBlock block)


-- TODO: Change assembly code header from Z to POWER
mathLibPowerHeader :: (String,StdGen) -> Bool -> [BS.ByteString]
mathLibPowerHeader (name0,stdgen) doublePrecision =
  let
    precisionBytes = if doublePrecision then 8 else 4
    name = BS.pack name0
  in [
     ".set r0,0; .set SP,1; .set RTOC,2; .set r3,3; .set r4,4"
    ,".set r5,5; .set r6,6; .set r7,7; .set r8,8; .set r9,9"
    ,".set r10,10; .set r11,11; .set r12,12; .set r13,13; .set r14,14"
    ,".set r15,15; .set r16,16; .set r17,17; .set r18,18; .set r19,19"
    ,".set r20,20; .set r21,21; .set r22,22; .set r23,23; .set r24,24"
    ,".set r25,25; .set r26,26; .set r27,27; .set r28,28; .set r29,29"
    ,".set r30,30; .set r31,31"
    ,".set v0,0; .set v1,1; .set v2,2; .set v3,3; .set v4,4"
    ,".set v5,5; .set v6,6; .set v7,7; .set v8,8; .set v9,9"
    ,".set v10,10; .set v11,11; .set v12,12; .set v13,13; .set v14,14"
    ,".set v15,15; .set v16,16; .set v17,17; .set v18,18; .set v19,19"
    ,".set v20,20; .set v21,21; .set v22,22; .set v23,23; .set v24,24"
    ,".set v25,25; .set v26,26; .set v27,27; .set v28,28; .set v29,29"
    ,".set v30,30; .set v31,31; .set v32,32; .set v33,33; .set v34,34"
    ,".set v35,35; .set v36,36; .set v37,37; .set v38,38; .set v39,39"
    ,".set v40,40; .set v41,41; .set v42,42; .set v43,43; .set v44,44"
    ,".set v45,45; .set v46,46; .set v47,47; .set v48,48; .set v49,49"
    ,".set v50,50; .set v51,51; .set v52,52; .set v53,53; .set v54,54"
    ,".set v55,55; .set v56,56; .set v57,57; .set v58,58; .set v59,59"
    ,".set v60,60; .set v61,61; .set v62,62; .set v63,63"
    ,".set BO_ALWAYS,20; .set CR0_LT,0"


    ,"    .csect .text[PR],2"
    ,"    .file \""<>name<>".c\",\"IBM Open XL C/C++ for AIX 17.1.1 (5725-C72, 5765-J18), version 17.1.1.4, LLVM version 15.0.0git\""
    ,"    .globl "<>name<>"[DS]"
    ,"    .globl  ."<>name
    ,"    .align  4"
    ,"    .csect "<>name<>"[DS],3"
    ,"    .vbyte  8, ."<>name
    ,"    .vbyte  8, TOC[TC0]"
    ,"    .vbyte  8, 0"
    ,"    .csect .text[PR],2"
    ,"."<>name<>":"
    ,"    subi r12,SP,152 # compute gpr save pointer, 152 = 8 bytes * 19 gprs"
    ,"    std     r13,-76(r12)             #save r13"
    ,"    std     r14,-72(r12)             #save r14"
    ,"    std     r15,-68(r12)             #save r15"
    ,"    std     r16,-64(r12)             #save r16"
    ,"    std     r17,-60(r12)             #save r17"
    ,"    std     r18,-56(r12)             #save r18"
    ,"    std     r19,-52(r12)             #save r19"
    ,"    std     r20,-48(r12)             #save r20"
    ,"    std     r21,-44(r12)             #save r21"
    ,"    std     r22,-40(r12)             #save r22"
    ,"    std     r23,-36(r12)             #save r23"
    ,"    std     r24,-32(r12)             #save r24"
    ,"    std     r25,-28(r12)             #save r25"
    ,"    std     r26,-24(r12)             #save r26"
    ,"    std     r27,-20(r12)             #save r27"
    ,"    std     r28,-16(r12)             #save r28"
    ,"    std     r29,-12(r12)             #save r29"
    ,"    std     r30,-8(r12)              #save r30"
    ,"    std     r31,-4(r12)              #save r31"
    ,"    stfd     v14,-144(SP)             #save v14"
    ,"    stfd     v15,-136(SP)             #save v15"
    ,"    stfd     v16,-128(SP)             #save v16"
    ,"    stfd     v17,-120(SP)             #save v17"
    ,"    stfd     v18,-112(SP)             #save v18"
    ,"    stfd     v19,-104(SP)             #save v19"
    ,"    stfd     v20,-96(SP)              #save v20"
    ,"    stfd     v21,-88(SP)              #save v21"
    ,"    stfd     v22,-80(SP)              #save v22"
    ,"    stfd     v23,-72(SP)              #save v23"
    ,"    stfd     v24,-64(SP)              #save v24"
    ,"    stfd     v25,-56(SP)              #save v25"
    ,"    stfd     v26,-48(SP)              #save v26"
    ,"    stfd     v27,-40(SP)              #save v27"
    ,"    stfd     v28,-32(SP)              #save v28"
    ,"    stfd     v29,-24(SP)              #save v29"
    ,"    stfd     v30,-16(SP)              #save v30"
    ,"    stfd     v31,-8(SP)               #save v31"
    -- ,"    stu        SP,-80(SP)"  -- store stack pointer
    ,"    lwz          r5,0(r5)"    -- load size from pointer
    ,"    mulli      r5,r5," <> (BS.pack $ show precisionBytes)     -- 8 bytes per double
    -- ,"    l          r6,T.22.constants(RTOC)" -- load constant pointer
   ]
    --  "* " <> (BS.pack $ show stdgen)
    -- ,(BS.pack $ map toUpper name) <> "   CELQPRLG BASEREG=NONE,DSASIZE=MYDSASZ,LEAF=YES"
    -- ,(BS.pack $ map toUpper name) <> "   ALIAS C'"<>BS.pack name<>"'"
    -- ,"    USING MYDSA,4         * establish addressibility to DSA"
    -- ,"    VSTM  V16,V23,VRSAVE   * save caller's V16 - V23 (non_volatile)"
    -- ,"    STD   8,FRSAVE        * save caller's FPR 8 - 15 (non_volatile)"
    -- ,"    STD   9,FRSAVE+8      * only if you have clobbered F8 - F15 or"
    -- ,"    STD   10,FRSAVE+16    * V8 - V15"
    -- ,"    STD   11,FRSAVE+24"
    -- ,"    STD   12,FRSAVE+32"
    -- ,"    STD   13,FRSAVE+40"
    -- ,"    STD   14,FRSAVE+48"
    -- ,"    STD   15,FRSAVE+56"
    -- ,"    lgfi  R0,64  * R0 hardcoded as counter"
    -- ,"    llgf  R3,0(,R3) * load size into R3"
    -- ,"    sllg  R3,R3,3(R0)  * 8 bytes per double"
    -- ]

-- TODO: Change to POWER
mathLibPowerReturn name0 =
  let
    name = BS.pack name0
    in [
      "RETURN:"

    ,"    subi r12,SP,152 # compute gpr save pointer, 152 = 8 bytes * 19 gprs"
    ,"    ld    r13,-76(r12)             #restore r13"
    ,"    ld    r14,-72(r12)             #restore r14"
    ,"    ld    r15,-68(r12)             #restore r15"
    ,"    ld    r16,-64(r12)             #restore r16"
    ,"    ld    r17,-60(r12)             #restore r17"
    ,"    ld    r18,-56(r12)             #restore r18"
    ,"    ld    r19,-52(r12)             #restore r19"
    ,"    ld    r20,-48(r12)             #restore r20"
    ,"    ld    r21,-44(r12)             #restore r21"
    ,"    ld    r22,-40(r12)             #restore r22"
    ,"    ld    r23,-36(r12)             #restore r23"
    ,"    ld    r24,-32(r12)             #restore r24"
    ,"    ld    r25,-28(r12)             #restore r25"
    ,"    ld    r26,-24(r12)             #restore r26"
    ,"    ld    r27,-20(r12)             #restore r27"
    ,"    ld    r28,-16(r12)             #restore r28"
    ,"    ld    r29,-12(r12)             #restore r29"
    ,"    ld    r30,-8(r12)              #restore r30"
    ,"    ld    r31,-4(r12)              #restore r31"
    ,"    lfd     v14,-144(SP)              #restore r14"
    ,"    lfd     v15,-136(SP)              #restore r15"
    ,"    lfd     v16,-128(SP)              #restore r16"
    ,"    lfd     v17,-120(SP)              #restore r17"
    ,"    lfd     v18,-112(SP)              #restore r18"
    ,"    lfd     v19,-104(SP)              #restore r19"
    ,"    lfd     v20,-96(SP)               #restore r20"
    ,"    lfd     v21,-88(SP)               #restore r21"
    ,"    lfd     v22,-80(SP)               #restore r22"
    ,"    lfd     v23,-72(SP)               #restore r23"
    ,"    lfd     v24,-64(SP)               #restore r24"
    ,"    lfd     v25,-56(SP)               #restore r25"
    ,"    lfd     v26,-48(SP)               #restore r26"
    ,"    lfd     v27,-40(SP)               #restore r27"
    ,"    lfd     v28,-32(SP)               #restore r28"
    ,"    lfd     r29,-24(SP)               #restore r29"
    ,"    lfd     r30,-16(SP)               #restore r30"
    ,"    lfd     r31,-8(SP)                #restore r31"
    ,"    blr"
     -- TODO are the below lines necessary?
     ,"L.."<>name<>"0:"
     ,"    .vbyte  4, 0x00000000"
     ,"    .byte   0x00"
     ,"    .byte   0x09"
     ,"    .byte   0x22"
     ,"    .byte   0x40"
     ,"    .byte   0x00"
     ,"    .byte   0x00"
     ,"    .byte   0x03"
     ,"    .byte   0x01"
     ,"    .vbyte  4, 0x00000000"
     ,"    .vbyte  4, L.."<>name<>"0-."<>name
     ,"    .vbyte  2, 0x0006"
     ,"    .byte   \""<>name<>"\""
    ]
    --  "RETURN DS 0H"
    -- ,"    VLM  V16,V23,VRSAVE"
    -- ,"    LD   8,FRSAVE"
    -- ,"    LD   9,FRSAVE+8"
    -- ,"    LD   10,FRSAVE+16"
    -- ,"    LD   11,FRSAVE+24"
    -- ,"    LD   12,FRSAVE+32"
    -- ,"    LD   13,FRSAVE+40"
    -- ,"    LD   14,FRSAVE+48"
    -- ,"    LD   15,FRSAVE+56"
    -- ,"    CELQEPLG"
    --             ]

  -- VST V5,FIFOS+8     TO SPILL
  -- VL  V5,FIFOS+8     TO RESTORE

mathLibPowerEnd = [
           "    .toc"
          ,"L..constants:"
          ,"    .tc constants[TC],constants[RW]"
          -- TODO need to add other memory regions here

       --  "# .data section"
       -- ,"        .toc                            # 0x00000100"
       -- ,"T.18.square:"
       -- ,"        .tc     H.18.square{TC},square{DS}"
       -- ,"T.22.constants:"
       -- ,"        .tc     H.22.constants{TC},constants{RO}"
       -- ,""
       -- ,"        .csect  square{DS}"
       -- ,"        .long   .square                 "
       -- ,"        .long   TOC{TC0}                "
       -- ,"        .long   0x00000000              "
       ]
  
  --    "MYDSA    DSECT"
  --   ,"VRSAVE   DS    8L" -- 0
  --   ,"FRSAVE   DS    8D" -- 8*16
  --   ,"SCRATCH  DS    8D" -- 8*16 + 8*8     TODO instead of 8D needs to be unrollD
  --   ,"FIFOS    DS    16L" -- NOTE should be calculated based on how many fifos there are
  --   ,"MYDSASZ  EQU   *-MYDSA"
  --   ,"R0       EQU   0"
  --   ,"R1       EQU   1"
  --   ,"R2       EQU   2"
  --   ,"R3       EQU   3"
  --   ,"R4       EQU   4"
  --   ,"R5       EQU   5"
  --   ,"R6       EQU   6"
  --   ,"R7       EQU   7"
  --   ,"R8       EQU   8"
  --   ,"R9       EQU   9"
  --   ,"R10       EQU   10"
  --   ,"R11       EQU   11"
  --   ,"R12       EQU   12"
  --   ,"R13       EQU   13"
  --   ,"R14       EQU   14"
  --   ,"R15       EQU   15"
  --   ,"V0       EQU   0"
  --   ,"V1       EQU   1"
  --   ,"V2       EQU   2"
  --   ,"V3       EQU   3"
  --   ,"V4       EQU   4"
  --   ,"V5       EQU   5"
  --   ,"V6       EQU   6"
  --   ,"V7       EQU   7"
  --   ,"V8       EQU   8"
  --   ,"V9       EQU   9"
  --   ,"V10       EQU   10"
  --   ,"V11       EQU   11"
  --   ,"V12       EQU   12"
  --   ,"V13       EQU   13"
  --   ,"V14       EQU   14"
  --   ,"V15       EQU   15"
  --   ,"V16       EQU   16"
  --   ,"V17       EQU   17"
  --   ,"V18       EQU   18"
  --   ,"V19       EQU   19"
  --   ,"V20       EQU   20"
  --   ,"V21       EQU   21"
  --   ,"V22       EQU   22"
  --   ,"V23       EQU   23"
  --   ,"V24       EQU   24"
  --   ,"V25       EQU   25"
  --   ,"V26       EQU   26"
  --   ,"V27       EQU   27"
  --   ,"V28       EQU   28"
  --   ,"V29       EQU   29"
  --   ,"V30       EQU   30"
  --   ,"V31       EQU   31"
  --   ,"    END"
  -- ]


--------------------------------------------------------------------
-- * Iterate Stochastic Code Generation

runCodeGen :: ([Char], CodeGraph POWER, Int)
    -> [HashedPenalty POWER]
    -> [HashedConstraint POWER]
    -> Int
    -> Integer
    -> IO ()
runCodeGen wrappedCG penalties constraints numStages n = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocatePower moduloCG unroll powerMetaData penalties constraints Nothing False defaultSolver defaultStages defaultFixedRegs
  let (code,tables) = codeGenCG cg (regMap,spillMap,schedGraph) powerMetaData
      allCode = mathLibPowerHeader (name,stdgen') doublePrecision ++ code
              ++ mathLibPowerReturn name ++ tables ++ mathLibPowerEnd
  BS.writeFile ("asm/"++name++"/"++name'++".s") $ BS.unlines allCode
  putStrLn $ "Wrote " ++ name' ++ ".s"
  where
    doublePrecision = False
    (name,moduloCG0,unroll) = wrappedCG
    name' = name++padN
    padN = let sN = show n
           in replicate (4 - length sN) '0' ++ sN
    moduloCG = cgApplyRewrites moduloCG0

iterateCodeGen wrappedCG penalties constraints numStages n
  | n < 0 = putStrLn "Finished"
  | otherwise = do m <- try $ do putStrLn $ "running codegen iteration: " ++ show n
                                 runCodeGen wrappedCG penalties constraints numStages n
                   case m of
                     Right _ -> do putStrLn $ "\n\nSuccessfully completed codegen iteration: "  ++ show n
                                   iterateCodeGen wrappedCG penalties constraints numStages (n-1)
                     Left (e :: SomeException) -> do putStrLn $ "Exception raised " ++ show e
                                                     iterateCodeGen wrappedCG penalties constraints numStages n
