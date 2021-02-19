{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ConstraintKinds            #-}

module Harakiri.Expr.Interpreter (interpret) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Fix
import Data.Functor.Compose
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)
import Data.Text.Read (signed, decimal)
import Prelude hiding (seq)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Harakiri.SourceCode
import Harakiri.Expr.Annotated
import Harakiri.Expr.Types
import Harakiri.Utils

import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HashMap

type InterpretM m = ( MonadReader (InterpretContext m) m
                    , MonadError Text m
                    , MonadIO m
                    )

newtype IntM a = IntM
    { unIntM :: ReaderT (InterpretContext IntM) IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadReader (InterpretContext IntM)
               , MonadIO
               )

instance MonadError Text IntM where
    throwError = liftIO . ioError . userError . unpack
    catchError ma hdl = do
        ctx <- ask
        liftIO $ catchIOError (runReaderT (unIntM ma) ctx) $ \err -> do
            let hdlMa = hdl $ pack $ ioeGetErrorString err
            runReaderT (unIntM hdlMa) ctx

data InterpretContext m = InterpretContext
    { curPos      :: !SrcSpan
    , sourceCode  :: !SourceCode
    , functions   :: !(HashMap Text (FuncInfo m))
    , variables   :: !(IORef (HashMap Text Int))
    , isBreak     :: !(IORef Bool)
    , isReturn    :: !(IORef Bool)
    , returnValue :: !(IORef (Maybe Int))
    }

instance Has SourceCode (InterpretContext m) where
    getter = sourceCode

instance Has SrcSpan (InterpretContext m) where
    getter = curPos

data FuncInfo m = FuncInfo
    { infArgs :: ![Text]
    , infBody :: !(m (Maybe Int))
    }

interpret :: SourceCode -> [Function Text PosExpr] -> IO ()
interpret src funcs = do
    vars <- newIORef HashMap.empty
    brk <- newIORef False
    ret <- newIORef False
    retVal <- newIORef Nothing
    let ctx = initCtx vars brk ret retVal
    catchIOError (void $ runReaderT (unIntM callMain) ctx) (putStr . ioeGetErrorString)
  where callMain = void $ callFunction "main" []
        fInfos = foldl collectFunctions HashMap.empty funcs
        collectFunctions finfoMap func =
            let body = interpretPosExpr (funBody func)
                fInfo = FuncInfo { infArgs = funArgs func
                                 , infBody = body
                                 }
            in  HashMap.insert (funName func) fInfo finfoMap
        initCtx vars brk ret retVal = InterpretContext { curPos      = SrcSpan iniPos iniPos
                                                       , sourceCode  = src
                                                       , functions   = fInfos
                                                       , variables   = vars
                                                       , isBreak     = brk
                                                       , isReturn    = ret
                                                       , returnValue = retVal
                                                       }
        iniPos = SourcePos "" 1 1

interpretPosExpr :: InterpretM m => PosExpr -> m (Maybe Int)
interpretPosExpr = adi (interpretExprF . annotated . getCompose) setContext

interpretExprF :: InterpretM m => ExprF (m (Maybe Int)) -> m (Maybe Int)
interpretExprF = shouldSkip . \case
    IntLit i -> return (Just i)
    Var var -> do
        vars <- asks variables >>= liftIO . readIORef
        let mval = HashMap.lookup var vars
        maybe (prettyError $ "undefined variable " <> var) (return . Just) mval
    Neg mval -> do
        val <- mval >>= maybe (prettyError "trying to negate non-integer value") return
        return (Just $ negate val)
    Binop mval1 op mval2 -> do
        let msg = "non-integer value in binary opeartion " <> showBinop op
        val1 <- mval1 >>= maybe (prettyError msg) return
        case op of
            And | val1 == 0 -> return (Just 0)
            Or  | val1 /= 0 -> return (Just 1)
            _ -> do
                val2 <- mval2 >>= maybe (prettyError msg) return
                return (Just $ binopToFunc op val1 val2)
    Call fun args -> do
        let msg = "non-integer value in function argument"
        margs <- sequence <$> sequence args
        intArgs <- maybe (prettyError msg) return margs
        callFunction fun intArgs
    Echo args -> do
        forM_ args $ \case
            StrArg str -> liftIO $ TIO.putStr str
            ExprArg mval -> do
                let msg = "trying to print non-integer value"
                val <- mval >>= maybe (prettyError msg) return
                liftIO (putStr $ show val)
        return Nothing
    Input -> do
        line <- liftIO TIO.getLine
        case signed decimal line of
            Left{}      -> prettyError "trying to input non-integer value"
            Right (i,_) -> return (Just i)
    Assign var mval -> do
        val <- mval >>= maybe (prettyError "trying to assign non-integer value") return
        vars <- asks variables
        liftIO $ modifyIORef' vars (HashMap.insert var val)
        return Nothing
    If mcond mth mel -> do
        cond <- mcond >>= maybe (prettyError "non-integer condition") return
        if cond /= 0
           then void mth
           else maybe (return ()) void mel
        return Nothing
    While mcond mbody -> do
        while (whileCond mcond) (void mbody)
        return Nothing
    Break -> do
        brk <- asks isBreak
        liftIO $ writeIORef brk True
        return Nothing
    Seq seq -> last <$> sequence seq
    Return mval -> do
        val <- fromMaybe (return Nothing) mval
        retVal <- asks returnValue
        isRet <- asks isReturn
        liftIO $ writeIORef retVal val
        liftIO $ writeIORef isRet True
        return val
  where binopToFunc :: Binop -> Int -> Int -> Int
        binopToFunc = \case
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> div
            Gt  -> fromBoolOp (>)
            Lt  -> fromBoolOp (<)
            Ge  -> fromBoolOp (>=)
            Le  -> fromBoolOp (<=)
            Eq  -> fromBoolOp (==)
            Ne  -> fromBoolOp (/=)
            And -> fromBoolOp (\a b -> (a /= 0) && (b /= 0))
            Or  -> fromBoolOp (\a b -> (a /= 0) || (b /= 0))

        fromBoolOp :: (Int -> Int -> Bool) -> Int -> Int -> Int
        fromBoolOp op a b = if op a b then 1 else 0

        whileCond :: InterpretM m => m (Maybe Int) -> m Bool
        whileCond mcond = do
            brkRef <- asks isBreak
            brk <- liftIO $ readIORef brkRef
            ret <- asks isReturn >>= liftIO . readIORef
            if brk || ret
               then do
                   liftIO $ writeIORef brkRef False
                   return False
               else mcond >>= maybe (prettyError "non-integer condition") (return . (/=0))

        shouldSkip :: InterpretM m => m (Maybe Int) -> m (Maybe Int)
        shouldSkip ma = do
            brk <- asks isBreak >>= liftIO . readIORef
            ret <- asks isReturn >>= liftIO . readIORef
            if brk || ret
               then return Nothing
               else ma

setContext :: InterpretM m => (PosExpr -> m (Maybe Int)) -> PosExpr -> m (Maybe Int)
setContext f expr =
    local (\ctx -> ctx { curPos = annotation $ getCompose $ unFix expr }) (f expr)

callFunction :: InterpretM m => Text -> [Int] -> m (Maybe Int)
callFunction fun args = do
    funcs <- asks functions
    case HashMap.lookup fun funcs of
        Nothing    -> prettyError $ "undefined function " <> fun
        Just fInfo -> call fInfo
  where call FuncInfo{infArgs = argNames, infBody = body}
          | actualNumArgs /= expectedNumArgs
          = prettyError $  "function " <> fun <> " expected " <> showText expectedNumArgs
                        <> " arguments but given " <> showText actualNumArgs
          | otherwise = do
              let argsMap = HashMap.fromList $ zip argNames args
              varsRef <- asks variables
              bakVars <- liftIO $ readIORef varsRef
              liftIO $ writeIORef varsRef argsMap
              void body
              liftIO $ writeIORef varsRef bakVars
              retValRef <- asks returnValue
              isRetRef <- asks isReturn
              retVal <- liftIO $ readIORef retValRef
              liftIO $ writeIORef retValRef Nothing
              liftIO $ writeIORef isRetRef False
              return retVal
          where actualNumArgs = length args
                expectedNumArgs = length argNames
