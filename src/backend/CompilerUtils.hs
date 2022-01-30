module CompilerUtils where

-- external
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List

-- internal
import Stack 
import AbsLatte
import Common
import DList
import Assemblyx86
import OrderedMap

data FuncScope = Global | Class MemberSource

data VarIndex = LocalVar Int | ClassVar Int | FuncArg Int | ClassMethodArg Int

instance Show VarIndex where
    show (LocalVar v) = "local_var " ++ show v
    show (ClassVar v) = "class_var " ++ show v
    show (FuncArg v) = "func_arg " ++ show v
    show (ClassMethodArg v) = "class_method_arg " ++ show v

type VarInfo = (VarType, VarIndex)
type FuncInfo = (FuncScope, FuncType)
type VariablesMap = Map.Map VarName VarInfo
type FunctionsMap = Map.Map FuncName FuncInfo
type ClassesMap = ClassesTypes
type BlockFrame = [VarInfo]

data CompilerState = CompilerState {
    generatedCode :: DList AssOp ,
    variables :: VariablesMap,
    scopeDeclaredVariables :: Set.Set VarName,
    functions :: FunctionsMap,
    classes :: ClassesTypes,
    constStrToLabel :: Map.Map String Label,
    customLabelToCount :: Map.Map Label Int,
    localVarCount :: Int,
    labelCount :: Int,
    constStrLabelCount :: Int,
    blocksFrames :: Stack BlockFrame -- stack of frame with indices of variables declared in current block
}

initCompilerState :: FunctionsMap -> ClassesMap -> CompilerState
initCompilerState functionsMap classesMap = CompilerState {
    generatedCode = DList.empty,
    variables = Map.empty,
    scopeDeclaredVariables = Set.empty,
    functions = functionsMap, 
    classes = classesMap,
    constStrToLabel = Map.empty,
    customLabelToCount = Map.empty,
    localVarCount = 0,
    labelCount = 0,
    constStrLabelCount = 0,
    blocksFrames = stackNew
}

type CompilerMonad = State CompilerState

emitLabel :: String -> CompilerMonad ()
emitLabel label = emitAssOp $ LABEL label 

emitAssOp :: AssOp -> CompilerMonad ()
emitAssOp op = do
    generatedCode' <- gets generatedCode
    modify (\state -> state {generatedCode = DList.append generatedCode' op})

emitAssOps :: [AssOp] -> CompilerMonad ()
emitAssOps = mapM_ emitAssOp

--------------- Getters --------------- 
getNewLabel :: CompilerMonad Label
getNewLabel = do
    id <- gets labelCount
    modify (\state -> state { labelCount = id + 1 })
    return (".label" ++ show id)

getNewLabelCustom :: String -> CompilerMonad Label
getNewLabelCustom name = do
    customLabelToCount' <- gets customLabelToCount
    let cnt = fromMaybe 0 (Map.lookup name customLabelToCount')
    modify (\state -> state { customLabelToCount = Map.insert name (cnt+1) customLabelToCount' })
    return ("." ++ name ++ "_" ++ show cnt)

-- If it's a new const string, then we have to define it,
-- otherwise we want to return label associated with this string
getConstStrLabel :: String -> CompilerMonad Label
getConstStrLabel str = do
    constStrToLabel' <- gets constStrToLabel
    case Map.lookup str constStrToLabel' of
        Just label -> return label
        Nothing -> do
            label <- generateNewLabel
            modify (\state -> state {
                constStrToLabel = Map.insert str label constStrToLabel'
            })
            return label
    where
        generateNewLabel = do
            id <- gets constStrLabelCount
            modify (\state -> state {constStrLabelCount = id + 1}) 
            return (".cstr" ++ show id)

getVTableLabel :: ClassName -> Label
getVTableLabel className = "_" ++ show className ++ "__vTable"

getMethodLabel :: ClassName -> FuncName -> Label
getMethodLabel className methodName = show className ++ "." ++ show methodName
 
-- we can be "unsafe" thanks to frontend
getVarType :: VarName -> CompilerMonad VarType
getVarType varName = do
    variables' <- gets variables
    let (type_, _) = (Map.!) variables' varName
    return type_

getVarIndex :: VarName -> CompilerMonad VarIndex
getVarIndex varName = do
    variables' <- gets variables
    let (_, varIdx) = (Map.!) variables' varName
    return varIdx

getNextLocalVarID :: CompilerMonad Int
getNextLocalVarID = do
    id <- gets localVarCount
    modify (\state -> state {
        localVarCount = id + 1
    })
    return id

-- object MemoryLoc + varOffset + dWord because of vTable + dWord for refCounter
getFieldMemoryOffset :: Int -> Int        
getFieldMemoryOffset fieldIdx = dWord * fieldIdx + (dWord * classFieldMemoryOffsetSize)

addLocalVar :: VarName -> TypeMeta -> CompilerMonad VarIndex
addLocalVar varName varType = do
    id <- getNextLocalVarID
    let varIdx = LocalVar id
    variables' <- gets variables
    scopeDeclaredVariables' <- gets scopeDeclaredVariables
    blocksFrames' <- gets blocksFrames
    -- Stack (bf:bfs) <- gets blocksFrames
    let Stack (bf:bfs) = blocksFrames'
    modify (\state -> state {
        variables = Map.insert varName (varType, varIdx) variables',
        scopeDeclaredVariables = Set.insert varName scopeDeclaredVariables',
        blocksFrames = stackPush (Stack bfs) ((varType, varIdx):bf)                            -- add variable idx to the current frame
    })
    return varIdx

-- https://stackoverflow.com/questions/14765406/function-prologue-and-epilogue-in-c
-- Prologue
-- push %ebp
-- mov %ebp, %esp
-- sub %esp, nLocalVars
-- ... Situation after prologue
-- ebp + 12:   arg2
-- ebp + 8:    arg1
-- ebp + 4:    Return address
-- ebp + 0:    Calling function's old ebp value
-- ebp - 4:    (local variables)
-- ...
-- 0-based indexing
getVarMemoryLoc :: VarIndex -> CompilerMonad Operand
getVarMemoryLoc (LocalVar idx) = return (MemoryLocOffset EBP ((-dWord) * (idx + 1)))
getVarMemoryLoc (ClassVar idx) = do
    emitAssOp $ MOVL (MemoryLocOffset EBP (2*dWord)) (Reg ECX)
    return (MemoryLocOffset ECX (dWord * (idx + 1)))  -- 1 for "this" == "vTable". Note that metadata is under 'negative offset'
getVarMemoryLoc (FuncArg idx) = return (MemoryLocOffset EBP (dWord * (idx + 2)))  -- 2 := old EBP + return address
-- +2 as above, because 'self' variable is added at index 0
getVarMemoryLoc (ClassMethodArg idx) = return (MemoryLocOffset EBP (dWord * (idx + 2))) -- as above +1 for vTable and -1 for "self" as method arg

getVarMemoryLocWith :: Register -> VarIndex -> CompilerMonad Operand
getVarMemoryLocWith _ (LocalVar idx) = return (MemoryLocOffset EBP ((-dWord) * (idx + 1)))
getVarMemoryLocWith reg (ClassVar idx) = do 
    emitAssOp $ MOVL (MemoryLocOffset EBP (2*dWord)) (Reg reg)
    return (MemoryLocOffset reg (dWord * (idx + 1)))  -- 1 for "this" == "vTable". Note that metadata is under 'negative offset'
getVarMemoryLocWith _ (FuncArg idx) = return (MemoryLocOffset EBP (dWord * (idx + 2)))  -- 2 := old EBP + return address
-- +2 as above, because 'self' variable is added at index 0
getVarMemoryLocWith _ (ClassMethodArg idx) = return (MemoryLocOffset EBP (dWord * (idx + 2))) -- as above +1 for vTable and -1 for "self" as method arg

------------------ Miscellaneous ------------------
getClassFieldInfo :: ClassName -> MemberName -> CompilerMonad (Int, VarType)
getClassFieldInfo className fieldName = do
    classMetadata <- getClassMetadata className
    let (idx, fieldType, _) = (OrderedMap.!) (fields classMetadata) fieldName
    return (idx, fieldType)


showBlockFrame :: BlockFrame -> String
showBlockFrame = intercalate "\n" . map (\(type_, varIdx) -> show varIdx ++ ": " ++ show type_)

setLocalVarCount :: Int -> CompilerMonad ()
setLocalVarCount val = modify (\state -> state { localVarCount = val })

pushBlockFrame :: BlockFrame -> CompilerMonad ()
pushBlockFrame bf = do
    blocksFrames' <- gets blocksFrames
    modify (\state -> state {
        blocksFrames = stackPush blocksFrames' bf
    })

popBlockFrame :: CompilerMonad BlockFrame 
popBlockFrame = do
    blocksFrames' <- gets blocksFrames
    let Stack (bf:bfs) = blocksFrames'
    modify (\state -> state {
        blocksFrames = Stack bfs
    })
    return bf

peekBlockFrame :: CompilerMonad BlockFrame
peekBlockFrame = do
    blocksFrames' <- gets blocksFrames
    let Stack (bf:_) = blocksFrames'
    return bf

variablesToBlockFrame :: VariablesMap -> BlockFrame
variablesToBlockFrame = map snd . Map.toList

getClassDtorLabel :: ClassName -> Label
getClassDtorLabel className = getMethodLabel (Ident ("__" ++ show className)) (Ident "dtor")

getClassMetadata :: ClassName -> CompilerMonad ClassMetadata
getClassMetadata className = do
    classes' <- gets classes
    return $ (Map.!) classes' className

-- dWord in bytes == 32-bits
dWord = 4

-- Utilities
libPrintInt = "printInt"
libIncrRefCounter = "__incr_ref_counter"
libDecrRefCounter = "__decr_ref_counter"
defaultDestructorLabel = "__default_destructor"
stringDestructorLabel = defaultDestructorLabel
arraySimpleDestructorLabel = defaultDestructorLabel -- for booleans and integers
arrayComplexDestructorLabel = "__array_destructor"  -- for other datatypes -> ref_count must be updated

classFieldMemoryOffsetSize = 1 -- for vTable == 'self'

refCounterOffset = -dWord
destructorOffset = -dWord * 2
metadataOffset = -dWord * 2

baseMetadataSize = 2 :: Int   -- reference count, destructor
classMetadataSize = 3 :: Int  -- vTable + metadata
arrayMetadataSize = 3 :: Int  -- length + metadata: 3 = +1 to store length, +1 to store refCount, +1 for destructor

labelInfoFnEntry = "__INFO_fn_entry"
labelInfoFnEnd = "__INFO_fn_end"
labelInfoRefCntInit = "__INFO_ref_cnt_init"
labelInfoDecrRefCntSafeEntry = "__INFO_decr_ref_cnt_safe_entry"
labelInfoDecrRefCntEntry = "__INFO_decr_ref_cnt_entry" 
labelInfoDecrRefCntEnd = "__INFO_decr_ref_cnt_end" 
infoLabels = [labelInfoFnEntry, labelInfoFnEnd, labelInfoRefCntInit, 
    labelInfoDecrRefCntSafeEntry, labelInfoDecrRefCntEntry, labelInfoDecrRefCntEnd]

isInfoLabel label = foldr(\infoLabel res -> infoLabel `isInfixOf` label || res) False infoLabels

whileEndLabel = "while_end"
