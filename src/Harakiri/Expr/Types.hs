{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData        #-}

module Harakiri.Expr.Types
    ( Function(..)
    , Expr
    , ExprF(..)
    , Binop(..)
    , EchoArg(..)
    , Type(..)

    , showFunction
    , showFunctionType
    , showExpr
    , showBinop
    , showType
    ) where

import Data.Fix
import Data.Functor.Classes (Eq1(..))
import Data.Text (Text, pack, null, intercalate)
import Prelude hiding (null, seq)

data Function a e = Function
    { funName :: Text
    , funArgs :: [a]
    , funType :: Type
    , funBody :: e
    } deriving (Eq, Functor, Foldable, Traversable)

showFunction :: Function Text Expr -> Text
showFunction fn =  "def " <> funName fn
                <> "(" <> intercalate "," (funArgs fn) <> ")"
                <> showFunctionType (funType fn)
                <> " {\n" <> showExpr (funBody fn) <> "\n}"

showFunctionType :: Type -> Text
showFunctionType = \case
    TVoid -> ""
    typ   -> ": " <> showType typ

type Expr = Fix ExprF

data ExprF e
    = IntLit Int
    | Var Text
    | Neg e
    | Binop e Binop e
    | Call Text [e]
    | Echo [EchoArg e]
    | Input
    | Assign Text e
    | If e e (Maybe e)
    | While e e
    | Break
    | Seq [e]
    | Return (Maybe e)
    deriving (Functor, Foldable, Traversable)

instance Eq1 ExprF where
    liftEq eq a b = case (a, b) of
        (IntLit i1, IntLit i2)                 -> i1 == i2
        (Var v1, Var v2)                       -> v1 == v2
        (Neg e1, Neg e2)                       -> eq e1 e2
        (Binop e11 op1 e21, Binop e12 op2 e22) -> op1 == op2
                                               && eq e11 e12
                                               && eq e21 e22
        (Call fn1 args1, Call fn2 args2)       -> fn1 == fn2
                                               && liftEq eq args1 args2
        (Echo args1, Echo args2)               -> liftEq (liftEq eq) args1 args2
        (Input, Input)                         -> True
        (Assign var1 val1, Assign var2 val2)   -> var1 == var2
                                               && eq val1 val2
        (If cond1 th1 el1, If cond2 th2 el2)   -> eq cond1 cond2
                                               && eq th1 th2
                                               && liftEq eq el1 el2
        (While cond1 body1, While cond2 body2) -> eq cond1 cond2
                                               && eq body1 body2
        (Break, Break)                         -> True
        (Seq seq1, Seq seq2)                   -> liftEq eq seq1 seq2
        (Return val1, Return val2)             -> liftEq eq val1 val2
        _                                      -> False

newtype PrefixFunc = PrefixFunc {apPrefixFunc :: Text -> Text -> Text}

showExpr :: Expr -> Text
showExpr expr = apPrefixFunc (foldFix showExprF expr) "" ""

showExprF :: ExprF PrefixFunc -> PrefixFunc
showExprF = \case
    IntLit i         -> showLeaf (showText i)
    Var v            -> showLeaf v
    Neg e            -> showNode "-" [e]
    Binop e1 op e2   -> showNode (showBinop op) [e1, e2]
    Call fn args     -> showNode (fn <> "()") args
    Echo args        -> showNode "echo()" (map showEchoArg args)
    Input            -> showLeaf "input()"
    Assign var val   -> showNode "=" [showLeaf var, val]
    If cond th mel   -> showNodeNames "if" $
        maybe [ ("", cond)
              , ("then", th)
              ]
              (\el -> [ ("", cond)
                      , ("then", th)
                      , ("else", el)
                      ]
              )
              mel
    While cond body  -> showNodeNames "while" [ ("", cond), ("do", body) ]
    Break            -> showLeaf "break"
    Seq seq          -> showNode ";" seq
    Return mret      -> showNode "return" $ maybe [] (\ret -> [ret]) mret

showNode :: Text -> [PrefixFunc] -> PrefixFunc
showNode node = showNodeNames node . map (\x -> ("",x))

showNodeNames :: Text -> [(Text, PrefixFunc)] -> PrefixFunc
showNodeNames node xs = PrefixFunc (\pr cpr -> pr <> node <> "\n" <> showXs pr cpr)
  where showXs :: Text -> Text -> Text
        showXs = snd (foldr (\a b -> (False, go a b)) (True, \_ _ -> "") xs)

        go :: (Text, PrefixFunc) -> (Bool, Text -> Text -> Text) -> Text -> Text -> Text
        go (name, PrefixFunc pfunc) (isLast, fs) pr cpr
          | null name = pfunc newPr newCpr <> rest
          | otherwise = showName <> rest
          where rest = fs pr cpr
                (newPr, newCpr) = if isLast
                                     then (cpr <> "└── ", cpr <> "    ")
                                     else (cpr <> "├── ", cpr <> "│   ")
                showName =  newPr <> name
                         <> "\n" <> pfunc (newCpr <> "└── ") (newCpr <> "    ")

showLeaf :: Text -> PrefixFunc
showLeaf s = PrefixFunc (\pr _ -> pr <> s <> "\n")

showEchoArg :: EchoArg PrefixFunc -> PrefixFunc
showEchoArg = \case
    StrArg  s -> showLeaf (showText s)
    ExprArg e -> e

showText :: Show a => a -> Text
showText = pack . show

data EchoArg e
    = StrArg Text
    | ExprArg e
    deriving (Functor, Foldable, Traversable)

instance Eq1 EchoArg where
    liftEq eq a b = case (a, b) of
        (StrArg s1, StrArg s2)   -> s1 == s2
        (ExprArg e1, ExprArg e2) -> eq e1 e2
        _                        -> False

data Binop
    = Add | Sub | Mul | Div
    | Lt | Gt | Le | Ge
    | Eq | Ne | And | Or
    deriving Eq

showBinop :: Binop -> Text
showBinop = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Lt  -> "<"
    Gt  -> ">"
    Le  -> "<="
    Ge  -> ">="
    Eq  -> "=="
    Ne  -> "!="
    And -> "&&"
    Or  -> "||"

data Type = TInt | TVoid deriving Eq

showType :: Type -> Text
showType = \case
    TInt    -> "int"
    TVoid   -> "void"
