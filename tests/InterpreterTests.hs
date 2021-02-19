module InterpreterTests (tests) where

import Control.Monad (zipWithM_)
import Data.Text (Text, lines, splitOn, replace, strip, dropAround)
import System.FilePath
import Test.HUnit (Test, assertEqual, assertFailure)
import Prelude hiding (lines, exp)

import Common
import Harakiri.SourceCode

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

data TestCase = TestCase
    { code     :: !SourceCode
    , input    :: ![Text]
    , expected :: ![Text]
    }

tests :: String -> (FilePath -> SourceCode -> IO ()) -> Test
tests name action = mkTestLabel name
    [ mkAssertion "tests/cases/case01.hk" "tests/outputs/case01"
    , mkAssertion "tests/cases/case02.hk" "tests/outputs/case02"
    , mkAssertion "tests/cases/case03.hk" "tests/outputs/case03"
    , mkAssertion "tests/cases/case04.hk" "tests/outputs/case04"
    , mkAssertion "tests/cases/case05.hk" "tests/outputs/case05"
    , mkAssertion "tests/cases/case06.hk" "tests/outputs/case06"
    , mkAssertion "tests/cases/case07.hk" "tests/outputs/case07"
    , mkAssertion "tests/cases/case08.hk" "tests/outputs/case08"
    ]
  where mkAssertion caseFile outputFile = do
            test <- readTestCase caseFile outputFile
            zipWithM_ (go test) (input test) (expected test)
          where go test inp exp = do
                    act <- getStdout $ setStdin inp $ action caseFile (code test)
                    assertEqual ("error on case " ++ caseFile) exp act

readTestCase :: FilePath -> FilePath -> IO TestCase
readTestCase caseFile outputFile = do
    src <- SourceCode <$> TIO.readFile caseFile
    outputs <- TIO.readFile outputFile
    (inputs, expecteds) <- unzip <$> mapM (toPair . splitOn "|") (lines outputs)
    return $ TestCase { code     = src
                      , input    = inputs
                      , expected = expecteds
                      }
  where toPair :: [Text] -> IO (Text, Text)
        toPair [a,b] = return (replace "," "\n" (strip a) <> "\n", parseExpected $ strip b)
        toPair _     = assertFailure ("error when parsing " ++ outputFile)

        parseExpected :: Text -> Text
        parseExpected exp
          | Text.length exp == 0 || Text.head exp /= '"' = exp
          | otherwise = dropAround (=='"') $ replace "\\n" "\n" exp
