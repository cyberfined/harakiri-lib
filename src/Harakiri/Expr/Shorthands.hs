module Harakiri.Expr.Shorthands
    ( mkIntLit
    , mkVar
    , mkNeg
    , mkBinop
    , mkCall
    , mkEcho
    , mkInput
    , mkAssign
    , mkIf
    , mkWhile
    , mkBreak
    , mkSeq
    , mkReturn
    , ($+), ($-), ($*), ($/)
    , ($==), ($!=), ($>), ($>=)
    , ($<), ($<=), ($&&), ($||)
    ) where

import Data.Fix
import Data.Text (Text)

import Harakiri.Expr.Types

mkIntLit :: Int -> Expr
mkIntLit = Fix . IntLit

mkVar :: Text -> Expr
mkVar = Fix . Var

mkNeg :: Expr -> Expr
mkNeg = Fix . Neg

mkBinop :: Binop -> Expr -> Expr -> Expr
mkBinop op e1 e2 = Fix (Binop e1 op e2)

mkCall :: Text -> [Expr] -> Expr
mkCall fn args = Fix (Call fn args)

mkEcho :: [EchoArg Expr] -> Expr
mkEcho = Fix . Echo

mkInput :: Expr
mkInput = Fix Input

mkAssign :: Text -> Expr -> Expr
mkAssign var val = Fix (Assign var val)

mkIf :: [Expr] -> Expr
mkIf [cond, th, el] = Fix (If cond th (Just el))
mkIf [cond, th]     = Fix (If cond th Nothing)
mkIf _              = error "mkIf takes 2 or 3 arguments"

mkWhile :: Expr -> Expr -> Expr
mkWhile cond body = Fix (While cond body)

mkBreak :: Expr
mkBreak = Fix Break

mkSeq :: [Expr] -> Expr
mkSeq = Fix . Seq

mkReturn :: Maybe Expr -> Expr
mkReturn = Fix . Return

($+), ($-), ($*), ($/), ($==), ($!=), ($>), ($>=), ($<), ($<=), ($&&), ($||)
    :: Expr -> Expr -> Expr
($+)  = mkBinop Add
($-)  = mkBinop Sub
($*)  = mkBinop Mul
($/)  = mkBinop Div
($==) = mkBinop Eq
($!=) = mkBinop Ne
($>)  = mkBinop Gt
($>=) = mkBinop Ge
($<)  = mkBinop Lt
($<=) = mkBinop Le
($&&) = mkBinop And
($||) = mkBinop Or
