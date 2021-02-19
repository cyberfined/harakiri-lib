module Harakiri.Compiler.LiveAnalysis
    ( LiveSet
    , liveAnalysis
    , inSet
    , outSet
    , isInCycle
    ) where

import Data.Array
import Data.IntMap (IntMap)
import Data.List (find)
import Data.Maybe (fromJust)

import Data.IntSet (IntSet, (\\))
import Harakiri.Expr.Types (Function(..))
import Harakiri.IR

import qualified Data.IntMap as IntMap

import qualified Data.IntSet as IntSet

data LiveSet = LiveSet
    { inSets    :: !(Array Int IntSet)
    , outSets   :: !(Array Int IntSet)
    , inCycle   :: !(Array Int Bool)
    } deriving (Eq, Show)

inSet :: Int -> LiveSet -> IntSet
inSet ind lset = inSets lset ! ind

outSet :: Int -> LiveSet -> IntSet
outSet ind lset = outSets lset ! ind

isInCycle :: Int -> LiveSet -> Bool
isInCycle ind lset = inCycle lset ! ind

liveAnalysis :: Enum a => Function a [IR a] -> LiveSet
liveAnalysis func = untilFix initLiveSet
  where indexedBody = zip [0..] $ funBody func
        cfg = createCFG (funBody func)
        cfgSize = snd (bounds cfg) + 1
        emptySets = listArray (bounds cfg) (replicate cfgSize IntSet.empty)
        initLiveSet = LiveSet emptySets emptySets cycleAnalysis

        cycleAnalysis :: Array Int Bool
        cycleAnalysis = foldl setCycle emptyCycles indexedBody
          where emptyCycles = listArray (bounds cfg) (replicate cfgSize False)
                setCycle cycles (ind, _)
                  | Just from <- find (< ind) (cfg ! ind)
                  = cycles // map (\i -> (i, True)) [from..ind]
                  | otherwise = cycles

        iteration :: Enum a => LiveSet -> (Int, IR a) -> LiveSet
        iteration lset (ind, instr) =
            lset { inSets  = inSets lset // [(ind, updIn)]
                 , outSets = outSets lset // [(ind, updOut)]
                 }
          where curOut = outSet ind lset
                updIn = use instr <> (curOut \\ def instr)
                updOut = mconcat $ map (\s -> inSet s lset) $ cfg ! ind

        untilFix :: LiveSet -> LiveSet
        untilFix oldSet
          | oldSet == newSet = newSet
          | otherwise        = untilFix newSet
          where newSet = foldl iteration oldSet indexedBody

createCFG :: Enum a => [IR a] -> Array Int [Int]
createCFG body = listArray (0, bodyLen-1) $ map go indexedBody
  where bodyLen = length body
        indexedBody = zip [0..] body

        go :: Enum a => (Int, IR a) -> [Int]
        go (ind, instr) = case instr of
            Branch (L lbl)         -> [fromJust $ IntMap.lookup lbl labels]
            BranchIf _ _ _ (L lbl)
              | ind == bodyLen-1   -> [fromJust $ IntMap.lookup lbl labels]
              | otherwise          -> [ind+1, fromJust $ IntMap.lookup lbl labels]
            Return{}               -> []
            _ | ind == bodyLen-1   -> []
              | otherwise          -> [ind+1]

        labels :: IntMap Int
        labels = foldl collectLabels IntMap.empty indexedBody

        collectLabels :: Enum a => IntMap Int -> (Int, IR a) -> IntMap Int
        collectLabels lbls (ind, instr) = case instr of
            Label (L lbl) -> IntMap.insert lbl ind lbls
            _             -> lbls

use :: Enum a => IR a -> IntSet
use = \case
    Neg _ src              -> tempToBitset src
    Binop _ _ src1 src2    -> tempToBitset src1 <> opToBitset src2
    Move _ src             -> moveOpToBitset src
    CallFunc _ _ args      -> mconcat $ map opToBitset args
    CallProc _ args        -> mconcat $ map opToBitset args
    Echo src               -> echoOpToBitset src
    Load _ src             -> opToBitset src
    Save src dst           -> tempToBitset src <> opToBitset dst
    BranchIf _ src1 src2 _ -> tempToBitset src1 <> opToBitset src2
    Return msrc            -> maybe IntSet.empty opToBitset msrc
    _                      -> IntSet.empty
  where opToBitset = \case
            Temp dst -> IntSet.singleton (fromEnum dst)
            _        -> IntSet.empty
        moveOpToBitset = \case
            MoveTemp dst -> IntSet.singleton (fromEnum dst)
            _            -> IntSet.empty
        echoOpToBitset = \case
            EchoTemp dst -> IntSet.singleton (fromEnum dst)
            _            -> IntSet.empty

def :: Enum a => IR a -> IntSet
def = \case
    Neg dst _        -> tempToBitset dst
    Binop _ dst _ _  -> tempToBitset dst
    Move dst _       -> tempToBitset dst
    Input dst        -> tempToBitset dst
    CallFunc dst _ _ -> tempToBitset dst
    Load dst _       -> tempToBitset dst
    _                -> IntSet.empty

tempToBitset :: Enum a => a -> IntSet
tempToBitset = IntSet.singleton . fromEnum
