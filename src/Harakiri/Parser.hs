module Harakiri.Parser (parseFromText) where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr
import Data.Char (isAlphaNum)
import Data.Fix
import Data.Functor (($>))
import Data.Text hiding (empty, cons, length)
import Data.Void
import Text.Megaparsec hiding (pos1)
import Text.Megaparsec.Char hiding (space, newline)

import Harakiri.Expr hiding (SourcePos(..), ($>))
import Harakiri.SourceCode

import qualified Data.HashSet as HashSet
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Harakiri.Expr as Expr

type Parser = Parsec Void Text

parseFromText :: String -> SourceCode -> Either Text [Function Text PosExpr]
parseFromText fpath src = case parse pRootExpr fpath (getSourceCode src) of
    Left err  -> Left $ pack $ errorBundlePretty err
    Right res -> Right res

pRootExpr :: Parser [Function Text PosExpr]
pRootExpr = (:) <$> pFunction <*> many pFunction <* eof

pFunction :: Parser (Function Text PosExpr)
pFunction =   Function
          <$> (space *> reserved "def" *> pId)
          <*> parens (sepBy pId (symbol ","))
          <*> return TVoid
          <*> braces pSeq

pExpr :: Parser PosExpr
pExpr = makeExprParser term operators <?> "expression"
  where term =   pIntLit
             <|> pInput
             <|> try pCall
             <|> pVar
             <|> parens pExpr

pIntLit :: Parser PosExpr
pIntLit = annotate1 (IntLit <$> lexeme Lexer.decimal <?> "integer")

pVar :: Parser PosExpr
pVar = annotate1 (Var <$> pId <?> "variable")

operators :: [[Operator Parser PosExpr]]
operators = [ [negop]
            , [binop "*" Mul, binop "/" Div]
            , [binop "+" Add, binop "-" Sub]
            , [ binop ">" Gt, binop ">=" Ge
              , binop "<" Lt, binop "<=" Le
              ]
            , [binop "==" Eq, binop "!=" Ne]
            , [binop "&&" And]
            , [binop "||" Or]
            ]
  where negop = Prefix (consNeg <$> annotate (operator "-"))
          where consNeg (Ann pos1 _) e@(AnnE pos2 _) = AnnE (pos1 <> pos2) (Neg e)
                consNeg _ _ = error "unexpected"
        binop op cons = InfixL (operator op $> consBinop)
          where consBinop e1@(AnnE pos1 _) e2@(AnnE pos2 _) =
                    AnnE (pos1 <> pos2) (Binop e1 cons e2)
                consBinop _ _ = error "unexpected"

operator :: Text -> Parser ()
operator ">" = void $ lexeme $ try (string ">" <* notFollowedBy (char '='))
operator "<" = void $ lexeme $ try (string "<" <* notFollowedBy (char '=' <|> char '>'))
operator n = void $ symbol n

pCall :: Parser PosExpr
pCall = annotate1
    (   Call
    <$> pId
    <*> parens (sepBy pExpr (symbol ","))
    <?> "function call"
    )

pEcho :: Parser PosExpr
pEcho = annotate1
    (   Echo
    <$> (reserved "echo" *> parens (sepBy pEchoArg (symbol ",")))
    <?> "echo call"
    )
  where pEchoArg :: Parser (EchoArg PosExpr)
        pEchoArg = StrArg <$> pStr <|> ExprArg <$> pExpr

        pStr :: Parser Text
        pStr = pack <$> (char '"' *> many (pChar <|> newline) <* symbol "\"") <?> "string"

        pChar :: Parser Char
        pChar = satisfy $ \x ->
            x == '!'               ||
            x == ' '               ||
            x >  '"'  && x <  '\\' ||
            x >  '\\' && x <= '~'

        newline :: Parser Char
        newline = char '\\' >> char 'n' >> return '\n'

pInput :: Parser PosExpr
pInput = annotate1 (Input <$ reserved "input" <* symbol "(" <* symbol ")")

pAssign :: Parser PosExpr
pAssign = annotate1
    (   Assign
    <$> pId
    <*> (operator "=" *> pExpr)
    <?> "assignment"
    )

pIf :: Parser PosExpr
pIf = annotate1
    (   If
    <$> (reserved "if" *> pExpr)
    <*> braces pSeq
    <*> optional (reserved "else" *> braces pSeq)
    <?> "if"
    )

pWhile :: Parser PosExpr
pWhile = annotate1
    (   While
    <$> (reserved "while" *> pExpr)
    <*> braces pSeq
    <?> "while"
    )

pBreak :: Parser PosExpr
pBreak = annotate1 (Break <$ reserved "break" <?> "break")

pSeq :: Parser PosExpr
pSeq = do
    beg <- getSourcePos
    x <- pStatement
    mxs <- optional ((:) <$> pStatement <*> many pStatement)
    end <- getSourcePos
    return $ case mxs of
        Nothing -> x
        Just xs -> AnnE (fromSourcePos beg end) (Seq $ x:xs)
  where pStatement =   try pAssign
                   <|> try pCall
                   <|> pEcho
                   <|> pIf
                   <|> pWhile
                   <|> pBreak
                   <|> pReturn

pReturn :: Parser PosExpr
pReturn = annotate1
    (   Return
    <$> (reserved "return" *> optional pExpr)
    <?> "return"
    )

reserved :: Text -> Parser ()
reserved = void . symbol

pId :: Parser Text
pId = lexeme $ do
    offset <- getOffset
    ident <-
        Text.cons
        <$> letterChar
        <*> takeWhileP Nothing (\x -> isAlphaNum x || x == '_')
    when (ident `HashSet.member` reservedWords) $
        throwError offset $ "unexpected reserved word " ++ unpack ident
    return ident

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

space :: Parser ()
space = Lexer.space space1 empty (Lexer.skipBlockComment "/*" "*/")

reservedWords :: HashSet.HashSet Text
reservedWords =
    HashSet.fromList [ "def", "return", "if", "while", "break", "else", "input", "echo" ]

annotate :: Parser a -> Parser (Ann SrcSpan a)
annotate p = do
    beg <- getSourcePos
    res <- p
    end <- getSourcePos
    return $ Ann (fromSourcePos beg end) res

annotate1 :: Functor f
          => Parser (f (Fix (AnnF SrcSpan f)))
          -> Parser (Fix (AnnF SrcSpan f))
annotate1 = fmap annToAnnF . annotate

fromSourcePos :: SourcePos -> SourcePos -> SrcSpan
fromSourcePos beg end = SrcSpan (convert beg) (convert end)
  where convert (SourcePos fpath ln cl) = Expr.SourcePos fpath (unPos ln) (unPos cl)

throwError :: Int -> String -> Parser a
throwError offset = parseError . FancyError offset . Set.singleton . ErrorFail
