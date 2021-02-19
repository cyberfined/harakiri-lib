module Main (main) where

import Test.HUnit
import qualified ParserTests
import qualified TypeCheckTests
import qualified IRTranslateTests

main :: IO ()
main = runTestTTAndExit $ TestList
    [ ParserTests.tests
    , TypeCheckTests.tests
    , IRTranslateTests.tests
    ]
