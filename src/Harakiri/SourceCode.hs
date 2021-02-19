module Harakiri.SourceCode (SourceCode(..)) where

import Data.Text (Text)

newtype SourceCode = SourceCode { getSourceCode :: Text }
