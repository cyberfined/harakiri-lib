{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Harakiri.IR.Interpreter (interpret) where

import Control.Monad.Reader
import Data.Array.IO
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.IORef
import Data.Text (Text, unpack)
import Data.Text.Read (signed, decimal)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Harakiri.Expr.Types (Function(..))
import Harakiri.IR.Translate (TransResult)
import Harakiri.IR.Types
import Harakiri.Utils (while, showText)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.Text.IO as TIO

import qualified Harakiri.IR.Translate as Trans

type InterpretM m = ( MonadReader (InterpretContext m) m
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

data InterpretContext m = InterpretContext
    { variables   :: !(IORef (IntMap Int))
    , strings     :: !(IntMap Text)
    , labels      :: !(IORef (IntMap Int))
    , progCounter :: !(IORef Int)
    , functions   :: !(HashMap Text (FuncInfo m))
    , returnValue :: !(IORef (Maybe Int))
    , isReturn    :: !(IORef Bool)
    }

data FuncInfo m = FuncInfo
    { infArgs :: ![Temp]
    , infBody :: !(m (Maybe Int))
    }

interpret :: TransResult -> IO ()
interpret Trans.TransResult { Trans.functions = funcs, Trans.strings = strs } = do
    vars <- newIORef IntMap.empty
    pc <- newIORef 0
    retVal <- newIORef Nothing
    isRet <- newIORef False
    (fInfos, labelMap) <-
        foldM (uncurry collectFunctions) (HashMap.empty, IntMap.empty) funcs
    labs <- newIORef labelMap
    let ctx = initCtx vars labs pc fInfos retVal isRet
    catchIOError (void $ runReaderT (unIntM callMain) ctx) (putStrLn . ioeGetErrorString)
  where callMain = callFunction "main" []
        collectFunctions fInfoMap labelMap func = do
            let body = funBody func
                bodyLen = length body
            bodyArr <- liftIO $ newListArray (0, bodyLen-1) body
            let fInfoMap' = HashMap.insert (funName func) (fInfo bodyArr) fInfoMap
                labelMap' = foldl collectLabels labelMap $ zip body [0..]
            return $ (fInfoMap', labelMap')
          where fInfo bodyArr = FuncInfo { infArgs = funArgs func
                                         , infBody = interpretFunction bodyArr
                                         }
        collectLabels labelMap (instr, ind) = case instr of
            Label (L lbl) -> IntMap.insert lbl (ind+1) labelMap
            _             -> labelMap
        initCtx vars lbl pc fInfos retVal isRet =
            InterpretContext { variables   = vars
                             , strings     = strs
                             , labels      = lbl
                             , progCounter = pc
                             , functions   = fInfos
                             , returnValue = retVal
                             , isReturn    = isRet
                             }

interpretFunction :: (Enum a, ShowReg a, InterpretM m)
                  => IOArray Int (IR a)
                  -> m (Maybe Int)
interpretFunction body = do
    bounds <- liftIO $ getBounds body
    let bodyLen = snd bounds + 1
    while (whileCond bodyLen) $ do
        pcRef <- asks progCounter
        pc <- liftIO $ readIORef pcRef
        liftIO $ modifyIORef' pcRef (+1)
        curCmd <- liftIO $ readArray body pc
        interpretIR curCmd
    retValRef <- asks returnValue
    isRetRef <- asks isReturn
    retVal <- liftIO $ readIORef retValRef
    liftIO $ writeIORef retValRef Nothing
    liftIO $ writeIORef isRetRef False
    return retVal
  where whileCond bodyLen = do
            isRet <- asks isReturn >>= liftIO . readIORef
            pc <- asks progCounter >>= liftIO . readIORef
            return (not isRet && pc < bodyLen)

interpretIR :: (Enum a, ShowReg a, InterpretM m) => IR a -> m ()
interpretIR = \case
    Neg dst src -> negate <$> readTemp src >>= setTemp dst
    Binop op dst src1 src2 ->
        apBinop op <$> readTemp src1 <*> readOperand src2 >>= setTemp dst
    Move dst src -> readMoveOperand src >>= setTemp dst
    Input dst -> do
        line <- liftIO TIO.getLine
        case signed decimal line of
            Left{}      -> throwError "trying to input non-integer value"
            Right (i,_) -> setTemp dst i
    CallFunc dst fun margs -> do
        args <- mapM readOperand margs
        mret <- callFunction fun args
        maybe (throwError "trying to assign non-integer value") (setTemp dst) mret
    CallProc fun margs -> do
        args <- mapM readOperand margs
        void $ callFunction fun args
    Echo eop -> case eop of
        EchoTemp t   -> readTemp t >>= liftIO . putStr . show
        EchoConst i  -> liftIO $ putStr $ show i
        EchoString s -> do
            strs <- asks strings
            case IntMap.lookup s strs of
                Nothing  -> throwError $ "wrong string reference " <> showText s
                Just str -> liftIO $ TIO.putStr $ str
    Load{}  -> throwError "can't interpret load"
    Save{}  -> throwError "can't interpret save"
    Label{} -> return ()
    Branch lbl -> do
        off <- getLabelOffset lbl
        pcRef <- asks progCounter
        liftIO $ writeIORef pcRef off
    BranchIf op src1 src2 lbl -> do
        res <- apRelop op <$> readTemp src1 <*> readOperand src2
        when (res /= 0) $ do
            off <- getLabelOffset lbl
            pcRef <- asks progCounter
            liftIO $ writeIORef pcRef off
    Return mret -> do
        isRetRef <- asks isReturn
        retValRef <- asks returnValue
        ret <- maybe (return Nothing) (fmap Just . readOperand) mret
        liftIO $ writeIORef isRetRef True
        liftIO $ writeIORef retValRef ret
  where setTemp :: (Enum a, ShowReg a, InterpretM m) => a -> Int -> m ()
        setTemp t val = do
            varsRef <- asks variables
            liftIO $ modifyIORef' varsRef (IntMap.insert (fromEnum t) val)

        getLabelOffset :: InterpretM m => Label -> m Int
        getLabelOffset l@(L lbl) = do
            lbls <- asks labels >>= liftIO . readIORef
            case IntMap.lookup lbl lbls of
                Nothing  -> throwError $ "undefined label " <> showLabel l
                Just off -> return off

        readOperand :: (Enum a, ShowReg a, InterpretM m) => Operand a -> m Int
        readOperand = \case
            Temp t  -> readTemp t
            Const i -> return i

        readMoveOperand :: (Enum a, ShowReg a, InterpretM m) => MoveOperand a -> m Int
        readMoveOperand = \case
            MoveTemp t   -> readTemp t
            MoveConst i  -> return i
            MoveString{} -> throwError "can't interpret MoveString"

        readTemp :: (Enum a, ShowReg a, InterpretM m) => a -> m Int
        readTemp tmp = do
            vars <- asks variables >>= liftIO . readIORef
            case IntMap.lookup (fromEnum tmp) vars of
                Nothing  -> throwError $ "undefined temp " <> showReg tmp
                Just val -> return val

        apBinop :: Binop -> Int -> Int -> Int
        apBinop = \case
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> div

        apRelop :: Relop -> Int -> Int -> Int
        apRelop = \case
            Gt -> relop (>)
            Lt -> relop (<)
            Ge -> relop (>=)
            Le -> relop (<=)
            Eq -> relop (==)
            Ne -> relop (/=)

        relop :: (Int -> Int -> Bool) -> Int -> Int -> Int
        relop op a b = if op a b then 1 else 0

callFunction :: InterpretM m => Text -> [Int] -> m (Maybe Int)
callFunction fun args = do
    mfInfo <- asks (HashMap.lookup fun . functions)
    case mfInfo of
        Nothing    -> throwError $ "undefined function " <> fun
        Just fInfo -> call fInfo
  where call fInfo
          | expectedNumArgs /= actualNumArgs
          = throwError $  "function " <> fun <> " expected " <> showText expectedNumArgs
                       <> " arguments but given " <> showText actualNumArgs
          | otherwise = do
              pcRef <- asks progCounter
              varsRef <- asks variables
              pcBak <- liftIO $ readIORef pcRef
              vars <- liftIO $ readIORef varsRef
              let newVars = foldl ins vars (zip args $ infArgs fInfo)
                  ins as (v,T a) = IntMap.insert a v as
              liftIO $ writeIORef varsRef newVars
              liftIO $ writeIORef pcRef 0
              res <- infBody fInfo
              liftIO $ writeIORef pcRef pcBak
              liftIO $ writeIORef varsRef vars
              return res
          where expectedNumArgs = length (infArgs fInfo)
                actualNumArgs = length args

throwError :: InterpretM m => Text -> m a
throwError = liftIO . ioError . userError . unpack
