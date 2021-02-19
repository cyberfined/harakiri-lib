module ParserTests (tests) where

import Data.Text (Text, unpack, unlines)
import Test.HUnit
import Prelude hiding (unlines, lines)

import Common
import Harakiri.Expr
import Harakiri.Parser
import Harakiri.SourceCode

tests :: Test
tests = mkTestLabel "parser tests"
    [ assertParse
        [ "def main() {"
        , "a = input()"
        , "b = input()"
        , "echo(\"a: \", a, \"b: \", b)"
        , "}"
        ]
        [ Function "main" [] TVoid $ mkSeq
            [ mkAssign "a" mkInput
            , mkAssign "b" mkInput
            , mkEcho [ StrArg "a: "
                     , ExprArg (mkVar "a")
                     , StrArg "b: "
                     , ExprArg (mkVar "b")
                     ]
            ]
        ]
    , assertParse
        [ "def print_interval(from, to) {"
        , "while from < to {"
        , "echo(from)"
        , "from = from + 1"
        , "}"
        , "}"
        , "def main() {"
        , "echo(\"enter a\")"
        , "a = input()"
        , "echo(\"enter b\")"
        , "b = input()"
        , "print_interval(a-2, b)"
        , "echo((a + b) * b)"
        , "echo(a + b * b)"
        , "}"
        ]
        [ Function "print_interval" ["from", "to"] TVoid $
            mkWhile (mkVar "from" $< mkVar "to") $
                mkSeq [ mkEcho [ExprArg (mkVar "from")]
                      , mkAssign "from" (mkVar "from" $+ mkIntLit 1)
                      ]
        , Function "main" [] TVoid $
            mkSeq [ mkEcho [StrArg "enter a"]
                  , mkAssign "a" mkInput
                  , mkEcho [StrArg "enter b"]
                  , mkAssign "b" mkInput
                  , mkCall "print_interval" [mkVar "a" $- mkIntLit 2, mkVar "b"]
                  , mkEcho [ExprArg ((mkVar "a" $+ mkVar "b") $* mkVar "b")]
                  , mkEcho [ExprArg (mkVar "a" $+ (mkVar "b" $* mkVar "b"))]
                  ]
        ]
    , assertParse
        [ "def main() {"
        , "a = 1"
        , "a = -a + 8"
        , "if a == 8 || a == 9 {"
        , "a = 90"
        , "} else { "
        , "if a == 10 {"
        , "a = 100"
        , "}"
        , "}"
        , "echo(a)"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkSeq [ mkAssign "a" (mkIntLit 1)
                  , mkAssign "a" (mkNeg (mkVar "a") $+ mkIntLit 8)
                  , mkIf [ (mkVar "a" $== mkIntLit 8) $|| (mkVar "a" $== mkIntLit 9)
                         , mkAssign "a" (mkIntLit 90)
                         , mkIf [ mkVar "a" $== mkIntLit 10
                                , mkAssign "a" (mkIntLit 100)
                                ]
                         ]
                  , mkEcho [ExprArg (mkVar "a")]
                  ]
        ]
    , assertParse
        [ "def main() {"
        , "while 1 {"
        , "a = input()"
        , "if a == 1 {"
        , "break"
        , "}"
        , "}"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkWhile (mkIntLit 1) $
                mkSeq [ mkAssign "a" mkInput
                      , mkIf [ mkVar "a" $== mkIntLit 1
                             , mkBreak
                             ]
                      ]
        ]
    , assertParse
        [ "def main() {"
        , "a = 5"
        , "echo(\"a = \", a, \"\\n\")"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkSeq [ mkAssign "a" (mkIntLit 5)
                  , mkEcho [ StrArg "a = "
                           , ExprArg (mkVar "a")
                           , StrArg "\n"
                           ]
                  ]
        ]
    , assertParse
        [ "def main() {"
        , "echo(5, \"\\n\\n\\n\")"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkEcho [ ExprArg (mkIntLit 5)
                   , StrArg "\n\n\n"
                   ]
        ]
    , assertParse
        [ "/*def main() {"
        , "*/"
        , "def main() {"
        , "echo(5)"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkEcho [ ExprArg (mkIntLit 5) ]
        ]
    , assertParse
        [ "def main() {"
        , "/* a = 5"
        , "   b = 7"
        , "*/"
        , "a = 7"
        , "b = 5"
        , "echo(a,b)"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkSeq [ mkAssign "a" (mkIntLit 7)
                  , mkAssign "b" (mkIntLit 5)
                  , mkEcho [ ExprArg (mkVar "a")
                           , ExprArg (mkVar "b")
                           ]
                  ]
        ]
    , assertParse
        [ ""
        , ""
        , ""
        , "/*a = 5*/"
        , "/*"
        , "def main() {}"
        , "*/"
        , "def main() {"
        , "a = 5"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkAssign "a" (mkIntLit 5)
        ]
    , assertParseFail
        [ "def main() {"
        , "123 = a"
        , "}"
        ]
    , assertParseFail
        [ "def main() {"
        , "if a = 123 {"
        , "a = 5"
        , "}"
        , "}"
        ]
    , assertParseFail
        [ "def main() {"
        , "input()"
        , "}"
        ]
    , assertParseFail
        [ "def main() {"
        , "a = input(1, 2)"
        , "}"
        ]
    , assertParseFail
        [ "def main() {"
        , "a = break"
        , "}"
        ]
    , assertParseFail
        [ "def main() {"
        , "if 5 < 6 {"
        , "a = 5"
        , "} else {"
        , "}"
        , "}"
        ]
    , assertParseFail
        [ "def main() {"
        , "if 5 < 6 {"
        , "} else {"
        , "a = 5"
        , "}"
        , "}"
        ]
    , assertParseFail
        [ "def main() {"
        , "if 5 < 6 {"
        , "} else {"
        , "}"
        , "}"
        ]
    ]

assertParse :: [Text] -> [Function Text Expr] -> Assertion
assertParse lines expected = case parseFromText "<string>" (SourceCode src) of
    Left err ->
        assertFailure $ "Unexpected error parsing `" ++ unpack src ++ "`:\n" ++ unpack err
    Right actual ->
        assertEqual ("When parsing " ++ unpack src)
            (ShowFunctions expected) (ShowFunctions $ map (fmap stripAnnotation) actual)
  where src = unlines lines

assertParseFail :: [Text] -> Assertion
assertParseFail lines = case parseFromText "<string>" (SourceCode src) of
    Left _ -> return ()
    Right res ->
        assertFailure $ "Unexpected success parsing `"
            ++ unpack src ++ "`:\nParsed value "
            ++ show (ShowFunctions $ map (fmap stripAnnotation) res)
  where src = unlines lines
