{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}

module Harakiri.TypeCheck
    ( TypedFunctions
    , getTypedFunctions
    , typeCheck
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Fix
import Data.Functor.Compose
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prelude hiding (seq)

import Harakiri.Expr
import Harakiri.SourceCode
import Harakiri.Utils

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

newtype TypedFunctions = TypedFunctions [Function Text PosExpr]

getTypedFunctions :: TypedFunctions -> [Function Text PosExpr]
getTypedFunctions (TypedFunctions funcs) = funcs

data FuncInfo = FuncInfo
    { numArgs    :: !Int
    , returnType :: !(Maybe Type)
    }

data CheckState = CheckState
    { funEnv          :: !(HashMap Text FuncInfo)
    , definedVars     :: !(HashSet Text)
    , curFinfo        :: !FuncInfo
    , returnInNonVoid :: !Bool
    }

data CheckContext = CheckContext
    { curFunName :: !Text
    , curPos     :: !SrcSpan
    , sourceCode :: !SourceCode
    , inLoop     :: !Bool
    , curNesting :: !Int
    }

instance Has SourceCode CheckContext where
    getter = sourceCode

instance Has SrcSpan CheckContext where
    getter = curPos

type TypeCheckM m = ( MonadReader CheckContext m
                    , MonadState CheckState m
                    , MonadError Text m
                    )

typeCheck :: SourceCode -> [Function Text PosExpr] -> Either Text TypedFunctions
typeCheck src funcs = TypedFunctions <$>
    runExcept (runReaderT (evalStateT checkAll initState) initCtx)
  where checkAll = do
            typedFuncs <- mapM checkFunction funcs
            env <- gets funEnv
            case HashMap.lookup "main" env of
                Nothing -> prettyError "function main is undefined"
                Just fInfo ->
                    when (numArgs fInfo /= 0) $
                        prettyError "main function must take 0 arguments"
            return typedFuncs
        checkFunction func = do
            let fName = funName func
                fInfo = FuncInfo { numArgs    = length $ funArgs func
                                 , returnType = Nothing
                                 }
                fBody = funBody func
            isExists <- checkFuncExists fName
            when isExists (prettyError $ "function " <> fName <> " is already defined")
            typeCheckPosExpr fName (funArgs func) fInfo fBody
            retInNonVoid <- gets returnInNonVoid
            unless retInNonVoid $
                prettyError $ "function " <> fName <> " must return a value"
            retType <- gets (returnType . curFinfo) >>= return . fromMaybe TVoid
            return func { funType = retType }
        initState = CheckState { funEnv          = HashMap.empty
                               , definedVars     = HashSet.empty
                               , curFinfo        = FuncInfo 0 Nothing
                               , returnInNonVoid = True
                               }
        initCtx = CheckContext { curPos     = SrcSpan iniPos iniPos
                               , inLoop     = False
                               , sourceCode = src
                               , curFunName = ""
                               , curNesting = 0
                               }
        iniPos = SourcePos "" 1 1

typeCheckPosExpr :: TypeCheckM m => Text -> [Text] -> FuncInfo -> PosExpr -> m ()
typeCheckPosExpr fName args fInfo posExpr
  | length args /= HashSet.size defVars
  = prettyError $ "duplicated arguments in function " <> fName
  | otherwise = local newCtx $ do
      modify newState
      void $ adi (typeCheckExprF . annotated . getCompose) setContext posExpr
  where defVars = HashSet.fromList args
        newState st = st { definedVars     = defVars
                         , funEnv          = HashMap.insert fName fInfo $ funEnv st
                         , returnInNonVoid = True
                         , curFinfo        = fInfo
                         }
        newCtx ctx = ctx { curNesting = 0
                         , inLoop     = False
                         , curFunName = fName
                         }

typeCheckExprF :: TypeCheckM m => ExprF (m Type) -> m Type
typeCheckExprF = \case
    IntLit{} -> return TInt
    Var var  -> do
        isExists <- checkVarExists var
        unless isExists (prettyError $ "undefined variable " <> var)
        return TInt
    Neg typ  -> typeMismatchError typ "trying negate non-integer typ"
    Binop typ1 op typ2 -> do
        void $ typeMismatchError typ1 msg
        void $ typeMismatchError typ2 msg
        return TInt
      where msg = "expected int in " <> showBinop op <> " operation"
    Call fn args -> do
        env <- gets funEnv
        case HashMap.lookup fn env of
            Nothing -> prettyError $ "undefined function " <> fn
            Just fInfo
              | length args /= numArgs fInfo
              -> prettyError $ "function " <> fn <> " expected "
                             <> showText (numArgs fInfo) <> " arguments"
                             <> " but given " <> showText (length args)
              | otherwise -> do
                  forM_ args $ \arg ->
                      typeMismatchError arg "non-integer arguments are not allowed"
                  return $ fromMaybe TVoid (returnType fInfo)
    Echo args -> do
        forM_ args $ \case
            StrArg{}  -> return ()
            ExprArg e -> void e
        return TVoid
    Input  -> return TInt
    Assign var val -> do
        void $ typeMismatchError val ("trying to assign non-integer value to " <> var)
        defineVar var
        return TVoid
    If cond th el -> do
        beforeIfVars <- gets definedVars
        void $ typeMismatchError cond "non-integer statement in if condition"
        void $ th
        modify (\st -> st { definedVars = beforeIfVars })
        maybe (return ()) void el
        modify (\st -> st { definedVars = beforeIfVars })
        return TVoid
    While cond body -> do
        beforeWhileVars <- gets definedVars
        void $ typeMismatchError cond "non-integer statement in while condition"
        void $ body
        modify (\st -> st { definedVars = beforeWhileVars })
        return TVoid
    Break -> do
        isInLoop <- asks inLoop
        unless isInLoop $ prettyError "break statement outside the while loop"
        return TVoid
    Seq seq -> do
        sequence_ seq
        return TVoid
    Return mval -> do
        actualType <- case mval of
            Nothing  -> return TVoid
            Just val -> do
                nesting <- asks curNesting
                setReturnInNonVoid (nesting == 0)
                typeMismatchError val " trying to return non-integer value"
        fInfo <- gets curFinfo
        fName <- asks curFunName
        case returnType fInfo of
            Nothing -> do
                let newfInfo = fInfo { returnType = Just actualType }
                updateFuncInfo newfInfo
            Just expectedType
              | actualType /= expectedType
              -> prettyError $  " wrong type of return statement in function " <> fName
                             <> ": expected " <> showType expectedType
                             <> " but given " <> showType actualType
              | otherwise -> return ()
        return TVoid

setContext :: TypeCheckM m => (PosExpr -> m Type) -> PosExpr -> m Type
setContext f = \case
    expr@(AnnE ann If{}) -> withinIf ann (f expr)
    expr@(AnnE ann While{}) -> withinLoop ann (f expr)
    expr -> setCurPos (annotation $ getCompose $ unFix expr) (f expr)
  where withinIf pos = local $ \ctx -> ctx { curNesting = curNesting ctx + 1
                                           , curPos = pos
                                           }
        withinLoop pos = local $ \ctx -> ctx { curNesting = curNesting ctx + 1
                                             , inLoop     = True
                                             , curPos     = pos
                                             }
        setCurPos pos = local (\ctx -> ctx { curPos = pos })

typeMismatchError :: TypeCheckM m => m Type -> Text -> m Type
typeMismatchError ma msg = do
    res <- ma
    when (res /= TInt) $ prettyError msg
    return res

updateFuncInfo :: TypeCheckM m => FuncInfo -> m ()
updateFuncInfo fInfo = do
    fName <- asks curFunName
    modify $ \st -> st { funEnv   = HashMap.insert fName fInfo $ funEnv st
                       , curFinfo = fInfo
                       }

checkFuncExists :: TypeCheckM m => Text -> m Bool
checkFuncExists fName = gets (HashMap.member fName . funEnv)

defineVar :: TypeCheckM m => Text -> m ()
defineVar var = modify (\st -> st { definedVars = HashSet.insert var $ definedVars st })

checkVarExists :: TypeCheckM m => Text -> m Bool
checkVarExists var = gets (HashSet.member var . definedVars)

setReturnInNonVoid :: TypeCheckM m => Bool -> m ()
setReturnInNonVoid val = modify (\st -> st { returnInNonVoid = val })
