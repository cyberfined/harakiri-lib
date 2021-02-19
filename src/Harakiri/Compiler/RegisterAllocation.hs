{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Harakiri.Compiler.RegisterAllocation
    ( AllocateResult(..)
    , allocateRegisters
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.IntSet (IntSet, (\\))
import Data.Foldable (toList)
import Data.List (sortBy, find, delete)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (<|), (|>))
import Data.Text (Text)

import Harakiri.Compiler.Architecture
import Harakiri.Compiler.LiveAnalysis
import Harakiri.Expr.Types (Function(..))
import Harakiri.IR.Translate (TransResult)
import Harakiri.IR.Types

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq

import qualified Harakiri.IR.Translate as Translate

type AllocateM a r m = ( Architecture a r
                       , MonadState (AllocateState r) m
                       , MonadReader (AllocateContext a r) m
                       , MonadError Text m
                       )

data AllocateState r = AllocateState
    { code           :: !(Seq (IR r))
    , tempPlaces     :: !(IntMap (TempPlace r))
    , lastTempRegs   :: !(IntMap r)
    , freeRegs       :: ![r]
    , busyParamRegs  :: !(IntMap Temp)
    , usedRegs       :: !IntSet
    , tempUsage      :: !(IntMap Int)
    , maxStackOffset :: !Int
    , curStackOffset :: !Int
    , firstOperand   :: !(Maybe r)
    , isFuncCall     :: !Bool
    , savedParams    :: !(IntMap Temp)
    }

data AllocateContext a r = AllocateContext
    { architecture     :: !a
    , curCommandIndex  :: !Int
    , lastCommandIndex :: !Int
    , minStackOffset   :: !Int
    , exitLabel        :: !Label
    , liveSet          :: !LiveSet
    , retRegister      :: !r
    }

data TempPlace r
    = InReg !r
    | InMem !Int
    | InRegMem !r !Int

combineTempPlaces :: TempPlace r -> TempPlace r -> TempPlace r
combineTempPlaces pl1 pl2 = case (pl1, pl2) of
    (inreg@(InReg{}), InReg{})          -> inreg
    (InReg inreg, InMem inmem)          -> InRegMem inreg inmem
    (InReg inreg, InRegMem _ inmem)     -> InRegMem inreg inmem
    (inmem@(InMem{}), InMem{})          -> inmem
    (InMem inmem, InReg inreg)          -> InRegMem inreg inmem
    (InMem inmem, InRegMem inreg _)     -> InRegMem inreg inmem
    (inregmem@(InRegMem{}), InRegMem{}) -> inregmem
    (InRegMem _ inmem, InReg inreg)     -> InRegMem inreg inmem
    (InRegMem inreg _, InMem inmem)     -> InRegMem inreg inmem

data AllocateResult r = AllocateResult
    { strings   :: !(IntMap Text)
    , functions :: ![Function r [IR r]]
    }

allocateRegisters :: Architecture a r
                  => a
                  -> TransResult
                  -> Either Text (AllocateResult r)
allocateRegisters arch transRes = case funcs of
    Left err       -> Left err
    Right newFuncs -> Right $ AllocateResult { functions = newFuncs
                                             , strings   = strs
                                             }
  where funcs = mapM (allocateFunction arch retReg) $ Translate.functions transRes
        strs = Translate.strings transRes
        paramRegs = paramRegisters arch
        regs = registers arch
        retReg = if null paramRegs
                    then head regs
                    else head paramRegs

allocateFunction :: Architecture a r
                 => a
                 -> r
                 -> Function Temp [IR Temp]
                 -> Either Text (Function r [IR r])
allocateFunction arch retReg func =
    runExcept (runReaderT (evalStateT run initState) initCtx)
  where run = do
            mapM_ (\(i, instr) -> setInd i (allocateIR instr)) $ zip [0..] body
            saveRegs
            allocBody <- gets code
            return $ func { funBody = toList allocBody
                          , funArgs = []
                          }
        initState = AllocateState { code = Seq.empty
                                  , tempPlaces = initTemps
                                  , lastTempRegs = lastRegs
                                  , freeRegs = registers arch
                                  , busyParamRegs = busyParams
                                  , usedRegs = IntSet.empty
                                  , tempUsage = IntMap.empty
                                  , maxStackOffset = stackOffset
                                  , curStackOffset = stackOffset
                                  , firstOperand = Nothing
                                  , isFuncCall = False
                                  , savedParams = IntMap.empty
                                  }
        initCtx = AllocateContext { architecture = arch
                                  , curCommandIndex = 0
                                  , lastCommandIndex = bodyLen - 1
                                  , liveSet = liveAnalysis func
                                  , retRegister = retReg
                                  , minStackOffset = stackOffset
                                  , exitLabel = L (maxLbl + 1)
                                  }

        body = funBody func
        bodyLen = length body
        args = funArgs func
        paramRegs = paramRegisters arch
        numParamRegs = length paramRegs
        stackOffset = wordSize arch * minStackOff
        (minStackOff, L maxLbl) = calcMinStackOffsetAndLabel body

        calcMinStackOffsetAndLabel = foldl go (0, L $ -1)
          where go (off, L lbl) = \case
                    CallFunc _ _ as -> (max (length as - numParamRegs) off, L lbl)
                    CallProc _ as   -> (max (length as - numParamRegs) off, L lbl)
                    Label (L l)     -> (off, L (max l lbl))
                    _               -> (off, L lbl)

        lastRegs = IntMap.foldlWithKey convertToLast IntMap.empty initTemps
        (initTemps, busyParams) = insertParams IntMap.empty IntMap.empty paramRegs args 0
        insertParams temps params (r:rs) (tmp@(T a):as) ind
          = insertParams newTemps newParams rs as ind
          where newTemps = IntMap.insert a (InReg r) temps
                newParams = IntMap.insert (fromEnum r) tmp params
        insertParams temps params _ ((T a):as) ind
          = insertParams newTemps params [] as (ind+1)
          where newTemps = IntMap.insert a (InMem $ -(ind+1) * wordSize arch) temps
        insertParams temps params _ _ _ = (temps, params)

        convertToLast lasts key = \case
            InMem{}        -> lasts
            InReg reg      -> IntMap.insert key reg lasts
            InRegMem reg _ -> IntMap.insert key reg lasts

        setInd i = local (\ctx -> ctx { curCommandIndex = i })

saveRegs :: AllocateM a r m => m ()
saveRegs = do
    arch <- asks architecture
    used <- gets usedRegs
    maxStackOff <- gets maxStackOffset
    let wordSz = wordSize arch
        paramSet = IntSet.fromList $ map fromEnum $ paramRegisters arch
        presRegs = preserveRegisters arch
        savedRegs = map toEnum (IntSet.toList $ used \\ paramSet) ++ presRegs
        savedRegsSize = wordSz * length savedRegs
        spSubSize = savedRegsSize + maxStackOff
        sp = stackPointer arch

    modify (\st -> st { code = fmap (mapStack wordSz spSubSize) $ code st })
    forM_ (zip [1..] savedRegs) $ \(i, r) -> do
        let off = maxStackOff + i*wordSz
        emitBeg (Save r (Const off))
        emitEnd (Load r (Const off))
    emitBeg (Binop Sub sp sp (Const spSubSize))
    emitEnd (Binop Add sp sp (Const spSubSize))
    emitEnd (Return Nothing)
  where mapStack wordSz size = \case
            Load dst (Const mem)
              | mem < 0 -> Load dst (Const $ size - mem - wordSz)
            Save src (Const mem)
              | mem < 0 -> Save src (Const $ size - mem - wordSz)
            op -> op
        emitBeg ir = modify (\st -> st { code = ir <| code st })
        emitEnd = emitIR

allocateIR :: AllocateM a r m => IR Temp -> m ()
allocateIR ir = resetOperand $ case ir of
    Neg dst src -> do
        srcReg <- loadTemp src
        freeRegisters
        dstReg <- getDstReg dst
        emitIR (Neg dstReg srcReg)
    Binop binop dst src1 src2 -> do
        hasImmDivMul <- asks (immDivMul . architecture)
        src1Reg <- loadTemp src1
        src2Op <- loadOperand src2
        immToReg <- case binop of
            Mul | hasImmDivMul -> return Nothing
                | otherwise -> case src2Op of
                    Const c -> return (Just c)
                    _       -> return Nothing
            Div | hasImmDivMul -> return Nothing
                | otherwise -> case src2Op of
                    Const c -> return (Just c)
                    _       -> return Nothing
            _ -> return Nothing

        case immToReg of
            Nothing -> do
                freeRegisters
                dstReg <- getDstReg dst
                emitIR (Binop binop dstReg src1Reg src2Op)
            Just c -> moveOperandToReg (MoveConst c) $ \r -> do
                freeRegisters
                dstReg <- getDstReg dst
                emitIR (Move r (MoveConst c))
                emitIR (Binop binop dstReg src1Reg (Temp r))
    Move dstTmp@(T dst) src -> do
        temps <- gets tempPlaces
        case IntMap.lookup dst temps of
            Nothing -> do
                dstReg <- getDstReg dstTmp
                moveToReg dstReg src
            Just place -> case place of
                InReg dstReg      -> moveToReg dstReg src
                InRegMem dstReg _ -> moveToReg dstReg src
                InMem dstMem -> do
                    moveOperandToReg src $ \srcReg ->
                        emitIR (Save srcReg (Const dstMem))
    Input dst -> do
        dstReg <- getDstReg dst
        retReg <- asks retRegister
        setArgs []
        emitIR (CallFunc retReg "input" [])
        when (retReg /= dstReg) $ emitIR (Move dstReg (MoveTemp retReg))
        unsaveParam dstReg
    CallFunc dst fn args -> do
        dstReg <- getDstReg dst
        retReg <- asks retRegister
        setArgs (map operandToMove args)
        emitIR (CallFunc retReg fn [])
        when (retReg /= dstReg) $ emitIR (Move dstReg (MoveTemp retReg))
        unsaveParam dstReg
    CallProc fn args -> do
        setArgs (map operandToMove args)
        emitIR (CallProc fn [])
    Echo src -> do
        let arg = echoToMove src
            func = case arg of
                MoveTemp{}   -> "echo_int"
                MoveConst{}  -> "echo_int"
                MoveString{} -> "echo_string"
        setArgs [arg]
        emitIR (CallProc func [])
    Load{} -> throwError "load allocation is unimplemented"
    Save{} -> throwError "save allocation is unimplemented"
    Label lbl  -> emitIR (Label lbl)
    Branch lbl -> emitIR (Branch lbl)
    BranchIf op src1 src2 lbl -> do
        src1Reg <- loadTemp src1
        src2Op <- loadOperand src2
        emitIR (BranchIf op src1Reg src2Op lbl)
    Return msrc -> do
        case msrc of
            Nothing -> return ()
            Just src -> do
                retReg <- asks retRegister
                srcOp <- loadOperand src
                case srcOp of
                    Const i              -> emitIR (Move retReg (MoveConst i))
                    Temp t | t == retReg -> return ()
                           | otherwise   -> emitIR (Move retReg (MoveTemp t))
        curCmd <- asks curCommandIndex
        lastCmd <- asks lastCommandIndex
        exitLbl <- asks exitLabel
        when (curCmd /= lastCmd) $ emitIR (Branch exitLbl)
  where resetOperand ma = do
            let newFuncCall = case ir of
                    Input{}    -> True
                    Echo{}     -> True
                    CallProc{} -> True
                    CallFunc{} -> True
                    _          -> False
            curFuncCall <- gets isFuncCall
            when (curFuncCall && not newFuncCall) loadSavedParams
            curCmd <- asks curCommandIndex
            lastCmd <- asks lastCommandIndex
            exitLbl <- asks exitLabel
            modify $ \st -> st { firstOperand = Nothing
                               , isFuncCall = newFuncCall
                               }
            void ma
            freeRegisters
            when (curCmd == lastCmd) $ emitIR (Label exitLbl)

        unsaveParam :: AllocateM a r m => r -> m ()
        unsaveParam reg = do
            let intReg = fromEnum reg
            modify (\st -> st { savedParams = IntMap.delete intReg $ savedParams st })

        loadSavedParams :: AllocateM a r m => m ()
        loadSavedParams = do
            let updateBusyParams st = IntMap.union (busyParamRegs st) (savedParams st)
            savedPs <- gets (IntMap.toList . savedParams)
            mapM_ (\(reg, tmp) -> moveToReg (toEnum reg) (MoveTemp tmp)) savedPs
            modify $ \st -> st { busyParamRegs = updateBusyParams st
                               , savedParams = IntMap.empty
                               }

        setArgs :: AllocateM a r m => [MoveOperand Temp] -> m ()
        setArgs args = do
            paramRegs <- asks (take numArgs . paramRegisters . architecture)
            busyParams <- gets busyParamRegs
            modify (\st -> st { savedParams = IntMap.union busyParams $ savedParams st })
            let savedRegs = mapMaybe (lookupParam busyParams) paramRegs
            mapM_ (\(r,t) -> storeRegister t (toEnum r)) $ IntMap.toList busyParams
            setArgs' args paramRegs savedRegs 0
            mapM_ (unregisterPlace . snd) $ IntMap.toList busyParams
          where numArgs = length args
                lookupParam m r = fmap (\t -> (r, t)) $ IntMap.lookup (fromEnum r) m
                setArgs' (a:as) (r:rs) savedRegs ind = do
                    argToReg r savedRegs a
                    setArgs' as rs savedRegs ind
                setArgs' (a:as) _ savedRegs ind = do
                    storeOperand a ind
                    setArgs' as [] savedRegs (ind + 1)
                setArgs' _ _ _ _ = return ()

argToReg :: AllocateM a r m => r -> [(r, Temp)] -> MoveOperand Temp -> m ()
argToReg dstReg savedRegs = \case
    MoveTemp srcTemp@(T src) -> do
        temps <- gets tempPlaces
        retReg <- asks retRegister
        case IntMap.lookup src temps of
            Nothing -> throwError $ "undefined temp " <> showTemp srcTemp
            Just place -> case place of
                InReg srcReg
                  | srcReg == dstReg -> when (dstReg == retReg) unReg
                  | otherwise -> do
                      unReg
                      emitIR (Move dstReg (MoveTemp srcReg))
                InRegMem srcReg _
                  | srcReg == dstReg -> when (dstReg == retReg) unReg
                  | otherwise -> do
                      unReg
                      emitIR (Move dstReg (MoveTemp srcReg))
                InMem srcMem -> do
                    unReg
                    loadRegister srcTemp dstReg srcMem
    MoveConst i  -> do
        unReg
        emitIR (Move dstReg (MoveConst i))
    MoveString s -> do
        unReg
        emitIR (Move dstReg (MoveString s))
  where unReg = case lookup dstReg savedRegs of
            Nothing -> return ()
            Just tmp -> unregisterPlace tmp

moveToReg :: AllocateM a r m => r -> MoveOperand Temp -> m ()
moveToReg dstReg = \case
    MoveTemp srcTemp@(T src) -> do
        temps <- gets tempPlaces
        case IntMap.lookup src temps of
            Nothing -> throwError $ "undefined temp " <> showTemp srcTemp
            Just place -> case place of
                InReg srcReg      -> emitIR (Move dstReg (MoveTemp srcReg))
                InRegMem srcReg _ -> emitIR (Move dstReg (MoveTemp srcReg))
                InMem srcMem      -> loadRegister srcTemp dstReg srcMem
    MoveConst i  -> emitIR (Move dstReg (MoveConst i))
    MoveString s -> emitIR (Move dstReg (MoveString s))

freeRegisters :: AllocateM a r m => m ()
freeRegisters = do
    arch <- asks architecture
    curCmd <- asks curCommandIndex
    lset <- asks liveSet
    temps <- gets tempPlaces
    regs <- gets freeRegs
    params <- gets busyParamRegs
    let freeTemps = IntSet.toList $ (inSet curCmd lset) \\ (outSet curCmd lset)
        initAccum = (regs, params, temps)
        (newFreeRegs, newBusyParams, newTempPlaces) =
            foldl (insertReg arch) initAccum freeTemps
    modify $ \st -> st { tempPlaces = newTempPlaces
                       , freeRegs = newFreeRegs
                       , busyParamRegs = newBusyParams
                       }
  where insertReg arch (regs, params, temps) tmp = case IntMap.lookup tmp temps of
            Nothing -> (regs, params, temps)
            Just place -> case place of
                InRegMem reg mem -> freeReg reg (IntMap.insert tmp (InMem mem) temps)
                InReg reg        -> freeReg reg (IntMap.delete tmp temps)
                InMem{}          -> (regs, params, temps)
          where freeReg reg newTemps
                  | reg `elem` (paramRegisters arch)
                  = (regs, IntMap.delete (fromEnum reg) params, newTemps)
                  | otherwise
                  = (reg:regs, params, newTemps)

loadMoveOperand :: AllocateM a r m => MoveOperand Temp -> m (MoveOperand r)
loadMoveOperand = \case
    MoveTemp t   -> MoveTemp <$> loadTemp t
    MoveConst i  -> return (MoveConst i)
    MoveString s -> return (MoveString s)

loadTemp :: AllocateM a r m => Temp -> m r
loadTemp tmp@(T t) = do
    temps <- gets tempPlaces
    resReg <- case IntMap.lookup t temps of
        Nothing -> throwError $ "undefined temp " <> showTemp tmp
        Just place -> case place of
            InReg reg      -> return reg
            InRegMem reg _ -> return reg
            InMem mem -> do
                reg <- getDstReg tmp
                loadRegister tmp reg mem
                return reg
    firstOp <- gets firstOperand
    case firstOp of
        Nothing -> modify (\st -> st { firstOperand = Just resReg })
        _       -> return ()
    return resReg

loadOperand :: AllocateM a r m => Operand Temp -> m (Operand r)
loadOperand = \case
    Const i  -> return (Const i)
    Temp tmp -> Temp <$> loadTemp tmp

getReg :: AllocateM a r m => m r
getReg = do
    regs <- gets freeRegs
    firstOp <- gets firstOperand
    case regs of
        (x:xs) -> do
            modify $ \st -> st { freeRegs = xs
                               , usedRegs = IntSet.insert (fromEnum x) $ usedRegs st
                               , firstOperand = maybe (Just x) Just $ firstOperand st
                               }
            return x
        _ -> do
            busyRegs <- getBusyRegs
            notFirstOp firstOp busyRegs
  where notFirstOp firstOp ((tx, rx):xs) = case firstOp of
            Nothing -> returnAndStore tx rx
            Just op
              | op == rx  -> notFirstOp firstOp xs
              | otherwise -> returnAndStore tx rx
        notFirstOp _ _ =  throwError "failed to get busy registers"
        returnAndStore tx rx = do
            storeRegister tx rx
            unregisterPlace tx
            return rx

getDstReg :: AllocateM a r m => Temp -> m r
getDstReg tmp@(T t) = do
    temps <- gets tempPlaces
    case IntMap.lookup t temps of
        Nothing -> regForTemp
        Just place -> case place of
            InReg reg      -> return reg
            InRegMem reg _ -> return reg
            InMem{}        -> regForTemp
  where regForTemp = do
            inCycle <- getInCycle
            lastRegs <- gets lastTempRegs
            busyParams <- gets busyParamRegs
            params <- asks (paramRegisters . architecture)
            reg <- case IntMap.lookup t lastRegs of
                Nothing -> getReg
                Just reg
                  | inCycle -> do
                      regs <- gets freeRegs
                      if reg `elem` regs
                         then modify (\st -> st { freeRegs = delete reg regs })
                         else case IntMap.lookup (fromEnum reg) busyParams of
                             Nothing
                               | reg `elem` params -> do
                                   let updateBusyParams = IntMap.insert (fromEnum reg) tmp
                                       newBusyParams = updateBusyParams busyParams
                                   modify (\st -> st { busyParamRegs = newBusyParams })
                               | otherwise -> do
                                   busyRegs <- getBusyRegs
                                   case find ((==reg) . snd) busyRegs of
                                       Just (tt, tr) -> do
                                           storeRegister tt tr
                                           unregisterPlace tt
                                       _ -> throwError "failed to get busy registers"
                             Just tt -> do
                                 storeRegister tt reg
                                 unregisterPlace tt
                      return reg
                  | otherwise -> getReg
            let updatePlaces = IntMap.insertWith combineTempPlaces t (InReg reg)
            modify (\st -> st { tempPlaces = updatePlaces $ tempPlaces st })
            return reg

getBusyRegs :: AllocateM a r m => m [(Temp, r)]
getBusyRegs = do
    usage <- gets tempUsage
    if not (IntMap.null usage)
       then getBusyRegs' usage
       else do
           curCmd <- asks curCommandIndex
           lastCmd <- asks lastCommandIndex
           lset <- asks liveSet
           let newUsage = foldl (collectUsage lset) IntMap.empty [curCmd..lastCmd]
           modify (\st -> st { tempUsage = newUsage })
           getBusyRegs' newUsage
  where getBusyRegs' :: AllocateM a r m => IntMap Int -> m [(Temp, r)]
        getBusyRegs' usage = do
            curCmd <- asks curCommandIndex
            iset <- asks (inSet curCmd . liveSet)
            temps <- gets tempPlaces
            let zero = 0 :: Int
                intersec = IntMap.intersection usage (IntMap.fromSet (const zero) iset)
                assocs = sortBy (\a b -> snd b `compare` snd a) $ IntMap.assocs intersec
                regs = foldl (go temps) [] assocs
            return regs
          where go temps regs (tmp, _) = case IntMap.lookup tmp temps of
                    Nothing -> regs
                    Just place -> case place of
                        InMem{}        -> regs
                        InReg reg      -> (T tmp, reg):regs
                        InRegMem reg _ -> (T tmp, reg):regs

        collectUsage :: LiveSet -> IntMap Int -> Int -> IntMap Int
        collectUsage lset usage ind = IntSet.foldl go usage (inSet ind lset)
          where go u i = IntMap.insertWith (+) i 1 u

emitIR :: AllocateM a r m => IR r -> m ()
emitIR ir = modify (\st -> st { code = code st |> ir })

storeOperand :: AllocateM a r m => MoveOperand Temp -> Int -> m ()
storeOperand op off = do
    wordSz <- asks (wordSize . architecture)
    moveOperandToReg op $ \srcReg -> emitIR (Save srcReg (Const $ wordSz * off))

storeRegister :: AllocateM a r m => Temp -> r -> m ()
storeRegister (T tmp) reg = do
    wordSz <- asks (wordSize . architecture)
    newStackOff <- gets ((+wordSz) . curStackOffset)
    let updatePlaces = IntMap.insertWith combineTempPlaces tmp (InMem newStackOff)
    modify $ \st -> st { curStackOffset = newStackOff
                       , maxStackOffset = max newStackOff (maxStackOffset st)
                       , tempPlaces = updatePlaces $ tempPlaces st
                       }
    emitIR (Save reg (Const newStackOff))

moveOperandToReg :: AllocateM a r m => MoveOperand Temp -> (r -> m b) -> m b
moveOperandToReg moveOp f = do
    op <- loadMoveOperand moveOp
    case op of
        MoveTemp r -> f r
        _ -> do
            r <- getReg
            emitIR (Move r op)
            modify (\st -> st { freeRegs = r : freeRegs st })
            f r

loadRegister :: AllocateM a r m => Temp -> r -> Int -> m ()
loadRegister (T tmp) reg off = do
    stackOff <- gets curStackOffset
    wordSz <- asks (wordSize . architecture)
    let newStackOff = if (stackOff == off && off > 0)
                         then stackOff - wordSz
                         else stackOff
        updatePlaces = IntMap.insertWith combineTempPlaces tmp (InReg reg)
    modify $ \st -> st { curStackOffset = newStackOff
                       , tempPlaces = updatePlaces $ tempPlaces st
                       , lastTempRegs = IntMap.insert tmp reg $ lastTempRegs st
                       }
    emitIR (Load reg (Const off))

getInCycle :: AllocateM a r m => m Bool
getInCycle = do
    curCmd <- asks curCommandIndex
    asks (isInCycle curCmd . liveSet)

unregisterPlace :: AllocateM a r m => Temp -> m ()
unregisterPlace (T tmp) = do
    temps <- gets tempPlaces
    case IntMap.lookup tmp temps of
        Nothing -> return ()
        Just place -> case place of
            InReg reg -> do
                modify (\st -> st { tempPlaces = IntMap.delete tmp $ tempPlaces st })
                freeReg reg
            InRegMem reg mem -> do
                let updatePlaces = IntMap.insert tmp (InMem mem)
                modify (\st -> st { tempPlaces = updatePlaces $ tempPlaces st })
                freeReg reg
            InMem{} -> return ()
  where freeReg reg = do
            params <- asks (paramRegisters . architecture)
            if reg `elem` params
               then do
                   let updateParams = IntMap.delete (fromEnum reg)
                   modify $ \st -> st { busyParamRegs = updateParams $ busyParamRegs st }
               else modify $ \st -> st { freeRegs = reg:freeRegs st }
