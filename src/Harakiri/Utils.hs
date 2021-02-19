{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Harakiri.Utils
    ( Has(..)
    , SourceCode(..)
    , adi
    , showText
    , prettyError
    , while
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Fix
import Data.Text (Text, pack)

import Harakiri.Expr.Annotated (SrcSpan(..), SourcePos(..))
import Harakiri.SourceCode

import qualified Data.Text as Text

class Has a t where
    getter :: t -> a

instance Has a a where
    getter = id

adi :: Functor f => (f a -> a) -> ((Fix f -> a) -> Fix f -> a) -> Fix f -> a
adi f g = g (f . fmap (adi f g) . unFix)

showText :: Show a => a -> Text
showText = pack . show

prettyError :: (Has SourceCode s, Has SrcSpan s, MonadReader s m, MonadError Text m)
            => Text
            -> m a
prettyError err = do
    SourcePos fpath ln cl <- asks (spanBegin . getter)
    src <- asks (getSourceCode . getter)
    let strLn = showText ln
        margin = Text.replicate (Text.length strLn + 2) " "
        errMsg =  Text.pack fpath <> ":" <> strLn <> ":" <> showText cl
               <> ": error:\n" <> err <> "\n"
        line = Text.takeWhile (/='\n')
             . (!!(ln-1))
             . iterate (Text.tail . Text.dropWhile (/= '\n')) $ src
        prettyLine =  margin <> "|\n " <> strLn <> " | " <> line <> "\n"
                   <> margin <> "|\n"
    throwError (errMsg <> prettyLine)

while :: Monad m => m Bool -> m () -> m ()
while mcond mbody = do
    cond <- mcond
    when cond (mbody >> while mcond mbody)
