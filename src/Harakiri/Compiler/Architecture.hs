{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Harakiri.Compiler.Architecture (Architecture(..)) where

import Data.Text (Text)

import Harakiri.Expr.Types (Function)
import Harakiri.IR.Types (IR, ShowReg(..))

class (Eq a, ShowReg b, Eq b, Enum b) => Architecture a b | a -> b where
    wordSize :: a -> Int
    registers :: a -> [b]
    paramRegisters :: a -> [b]
    preserveRegisters :: a -> [b]
    stackPointer :: a -> b
    immDivMul :: a -> Bool
    translateToAsm :: a -> [(Int, Text)] -> [Function b [IR b]] -> Text
