{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms   #-}

module Harakiri.Expr.Annotated
    ( -- Data types
      SourcePos(..)
    , SrcSpan(..)
    , Ann(..)
    , AnnF
    , PosExprF
    , PosExpr

      -- patterns
    , pattern AnnE
    , pattern While_

      -- functions
    , annToAnnF
    , stripAnnotation
    ) where

import Data.Function (on)
import Data.Functor.Compose
import Data.Fix

import Harakiri.Expr.Types

data SourcePos = SourcePos !String !Int !Int deriving (Eq, Show)

instance Ord SourcePos where
    (SourcePos _ l1 c1) `compare` (SourcePos _ l2 c2) =
        (l1 `compare` l2) <> (c1 `compare` c2)

data SrcSpan = SrcSpan { spanBegin :: !SourcePos
                       , spanEnd   :: !SourcePos
                       } deriving (Eq, Ord, Show)

instance Semigroup SrcSpan where
    s1 <> s2 = SrcSpan ((min `on` spanBegin) s1 s2) ((max `on` spanEnd) s1 s2)

data Ann ann a = Ann { annotation :: !ann
                     , annotated  :: !a
                     } deriving (Functor, Foldable, Traversable)

type AnnF ann f = Compose (Ann ann) f

annToAnnF :: Ann ann (f (Fix (AnnF ann f))) -> Fix (AnnF ann f)
annToAnnF = Fix . Compose

stripAnnotation :: Functor f => Fix (AnnF ann f) -> Fix f
stripAnnotation = unfoldFix (annotated . getCompose . unFix)

type PosExprF = AnnF SrcSpan ExprF

type PosExpr = Fix PosExprF

pattern AnnE :: ann -> f (Fix (AnnF ann f)) -> Fix (AnnF ann f)
pattern AnnE ann f = Fix (Compose (Ann ann f))

pattern While_ :: SrcSpan -> e -> e -> PosExprF e
pattern While_ ann cond body = Compose (Ann ann (While cond body))
