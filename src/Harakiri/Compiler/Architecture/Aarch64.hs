{-# LANGUAGE MultiParamTypeClasses #-}

module Harakiri.Compiler.Architecture.Aarch64
    ( Aarch64(..)
    , Register(..)
    ) where

import Data.Text (Text, intercalate, unlines)
import Prelude hiding (unlines)

import Harakiri.Compiler.Architecture
import Harakiri.Expr.Types (Function(..))
import Harakiri.IR.Types
import Harakiri.Utils

data Aarch64 = Aarch64 deriving Eq

instance Architecture Aarch64 Register where
    wordSize           _  = 8
    registers          _  = enumFromTo X8 X29
    paramRegisters     _  = enumFromTo X0 X7
    preserveRegisters  _  = [X30]
    stackPointer       _  = SP
    immDivMul          _  = False
    translateToAsm _ strs funcs =  stringsToAsm strs <> ".text\n.globl main\n"
                                <> funcsToAsm funcs

stringsToAsm :: [(Int, Text)] -> Text
stringsToAsm strings
  | null strs = ""
  | otherwise = section <> content <> "\n"
  where section = ".section .rodata\n"
        content = unlines $ strs
        strs = map str strings
        str (i, s) = "str" <> showText i <> ": .string " <> showText s

funcsToAsm :: [Function Register [IR Register]] -> Text
funcsToAsm = unlines . map funcToAsm
  where funcToAsm func = foldl (irToAsm lblPrefix) (fName <> ":\n") $ funBody func
          where fName = funName func
                lblPrefix = fName <> "_"

irToAsm :: Text -> Text -> IR Register -> Text
irToAsm lblPrefix code = \case
    Neg dst src -> emitCmd "neg" [showReg dst, showReg src]
    Binop op dst src1 src2 -> emitCmd (binopToAsm op) [ showReg dst
                                                      , showReg src1
                                                      , operandToAsm src2
                                                      ]
    Move dst src -> case src of
        MoveString s -> emitCmd "ldr" [showReg dst, "=" <> showStringLabel s]
        _            -> emitCmd "mov" [showReg dst, moveOperandToAsm src]
    Input{}         -> emitCmd "bl" ["input"]
    CallFunc _ fn _ -> emitCmd "bl" [fn]
    CallProc fn _   -> emitCmd "bl" [fn]
    Echo{}          -> emitCmd "bl" ["echo"]
    Load dst src    -> emitCmd "ldr" [showReg dst, "[sp, " <> operandToAsm src <> "]"]
    Save src dst    -> emitCmd "str" [showReg src, "[sp, " <> operandToAsm dst <> "]"]
    Label lbl       -> emitCmdNoTabs (labelToAsm lbl <> ":") []
    Branch lbl      -> emitCmd "b" [labelToAsm lbl]
    BranchIf op src1 src2 lbl -> emitCmds [ showCmd "cmp" [ showReg src1
                                                          , operandToAsm src2
                                                          ]
                                          , showCmd (relopToAsm op) [labelToAsm lbl]
                                          ]
    Return{} -> emitCmd "ret" ["x30"]
  where emitCmd :: Text -> [Text] -> Text
        emitCmd cmd ops = code <> showCmd cmd ops

        emitCmdNoTabs :: Text -> [Text] -> Text
        emitCmdNoTabs cmd ops = code <> showCmdNoTabs cmd ops

        emitCmds :: [Text] -> Text
        emitCmds cmds = code <> mconcat cmds

        showCmd :: Text -> [Text] -> Text
        showCmd cmd ops = "    " <> showCmdNoTabs cmd ops

        showCmdNoTabs :: Text -> [Text] -> Text
        showCmdNoTabs cmd ops = cmd <> " " <> intercalate ", " ops <> "\n"

        labelToAsm :: Label -> Text
        labelToAsm lbl = lblPrefix <> showLabel lbl

        binopToAsm :: Binop -> Text
        binopToAsm = \case
            Add -> "add"
            Sub -> "sub"
            Div -> "sdiv"
            Mul -> "mul"

        relopToAsm :: Relop -> Text
        relopToAsm = \case
            Lt -> "blt"
            Gt -> "bgt"
            Le -> "ble"
            Ge -> "bge"
            Eq -> "beq"
            Ne -> "bne"

        operandToAsm :: Operand Register -> Text
        operandToAsm = \case
            Temp r  -> showReg r
            Const c -> "#" <> showText c

        moveOperandToAsm :: MoveOperand Register -> Text
        moveOperandToAsm = \case
            MoveTemp r   -> showReg r
            MoveConst c  -> "#" <> showText c
            MoveString s -> "#" <> showStringLabel s

data Register
    = X0  | X1  | X2  | X3  | X4  | X5
    | X6  | X7  | X8  | X9  | X10 | X11
    | X12 | X13 | X14 | X15 | X16 | X17
    | X18 | X19 | X20 | X21 | X22 | X23
    | X24 | X25 | X26 | X27 | X28 | X29
    | X30 | SP
    deriving (Eq, Enum)

instance ShowReg Register where
    showReg = \case
        X0  -> "x0"
        X1  -> "x1"
        X2  -> "x2"
        X3  -> "x3"
        X4  -> "x4"
        X5  -> "x5"
        X6  -> "x6"
        X7  -> "x7"
        X8  -> "x8"
        X9  -> "x9"
        X10 -> "x10"
        X11 -> "x11"
        X12 -> "x12"
        X13 -> "x13"
        X14 -> "x14"
        X15 -> "x15"
        X16 -> "x16"
        X17 -> "x17"
        X18 -> "x18"
        X19 -> "x19"
        X20 -> "x20"
        X21 -> "x21"
        X22 -> "x22"
        X23 -> "x23"
        X24 -> "x24"
        X25 -> "x25"
        X26 -> "x26"
        X27 -> "x27"
        X28 -> "x28"
        X29 -> "x29"
        X30 -> "x30"
        SP  -> "sp"
