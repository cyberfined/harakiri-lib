module IRTranslateTests (tests) where

import Data.Text (unpack)
import Test.HUnit

import Harakiri.IR
import Harakiri.Parser
import Harakiri.SourceCode
import Harakiri.TypeCheck

import qualified InterpreterTests

tests :: Test
tests = InterpreterTests.tests "ir translation tests" parseAndInterpret

parseAndInterpret :: FilePath -> SourceCode -> IO ()
parseAndInterpret file src = case parseFromText file src of
    Left err ->
        assertFailure $ "Unexpected error parsing " ++ file ++ ":\n" ++ unpack err
    Right funcs -> case typeCheck src funcs of
        Left err ->
            assertFailure $  "Unexpected error type checking "
                          ++ file ++ ":\n" ++ unpack err
        Right typedFuncs -> case translateToIR typedFuncs of
            Left err -> assertFailure $  "Unexpected error translating "
                                      ++ file ++ ":\n" ++ unpack err
            Right transRes -> interpret transRes
