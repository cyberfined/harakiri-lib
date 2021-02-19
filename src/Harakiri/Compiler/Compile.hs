{-# LANGUAGE MultiParamTypeClasses #-}

module Harakiri.Compiler.Compile
    ( compile
    , compileAllocateResult
    ) where

import Data.Text (Text)

import Harakiri.Compiler.Architecture
import Harakiri.Compiler.RegisterAllocation(AllocateResult(..))
import Harakiri.Expr.Types (Function(..))
import Harakiri.IR.Types (IR)

import qualified Data.IntMap as IntMap

compile :: Architecture a r => a -> [(Int, Text)] -> [Function r [IR r]] -> Text
compile = translateToAsm

compileAllocateResult :: Architecture a r => a -> AllocateResult r -> Text
compileAllocateResult arch allocRes = compile arch strs (functions allocRes)
  where strs = IntMap.toList (strings allocRes)
