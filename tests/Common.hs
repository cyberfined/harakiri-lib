module Common
    ( ShowFunctions(..)
    , mkTestLabel
    , getStdout
    , setStdin
    ) where

import Control.Monad (void)
import Data.Text (Text, unpack, unlines)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Prelude hiding (unlines)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import Test.HUnit

import Harakiri.Expr

import qualified Data.Text.IO as TIO

newtype ShowFunctions = ShowFunctions
    { runShowFunctions :: [Function Text Expr]
    } deriving Eq

instance Show ShowFunctions where
    show = ('\n':) . unpack . unlines . map showFunction . runShowFunctions

mkTestLabel :: String -> [Assertion] -> Test
mkTestLabel lbl = TestLabel lbl . TestList . map TestCase

getStdout :: IO a -> IO Text
getStdout ma = do
    (tmpPath, tmpHdl) <- getTempFile "harakiri_stdout"
    stdDup <- hDuplicate stdout
    hDuplicateTo tmpHdl stdout
    hClose tmpHdl
    void ma
    hDuplicateTo stdDup stdout
    hClose stdDup
    content <- TIO.readFile tmpPath
    removeFile tmpPath
    return content

setStdin :: Text -> IO a -> IO a
setStdin inp ma = do
    (tmpPath, tmpHdl) <- getTempFile "harakiri_stdin"
    TIO.hPutStr tmpHdl inp
    hSeek tmpHdl AbsoluteSeek 0
    stdDup <- hDuplicate stdin
    hDuplicateTo tmpHdl stdin
    hClose tmpHdl
    res <- ma
    hDuplicateTo stdDup stdin
    hClose stdDup
    removeFile tmpPath
    return res

getTempFile :: String -> IO (FilePath, Handle)
getTempFile name = do
    tmpDir <- getTemporaryDirectory
    openTempFile tmpDir name
