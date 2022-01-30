{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Compiler where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe

import Debug.Trace

import Stack
import Common
import Assemblyx86
import AbsLatte
import CompilerUtils
import Utils
import DList
import qualified OrderedMap as OM
import qualified GarbageCollector as GC

type ReturnType = TypeMeta
type FuncArgs = [ArgMeta]
type FuncBody = BlockMeta
data Function = Function ReturnType FuncArgs FuncBody 
              | Method ReturnType FuncArgs FuncBody 

runCompiler :: ProgramMeta -> FunctionsTypes -> ClassesTypes -> CompilerState 
runCompiler (Program _ topDefs) functionsTypes classesTypes = execState (do 
        GC.emitGarbageCollector
        emitReadStringLibFunc
        forM_ topDefs emitTopDef) (initCompilerState (Map.map (Global, ) functionsTypes) classesTypes)

emitTopDef :: TopDefMeta -> CompilerMonad ()
emitTopDef (TopFuncDef _ (FuncDef _ retType (Ident funcName) args body)) = emitFunc (Function retType args body) funcName Nothing
emitTopDef (TopClassDef _ (ClassDef _ className classMembers)) = emitClass className classMembers
emitTopDef (TopClassDef _ (ClassExtDef _ className _ classMembers)) = emitTopDef (TopClassDef Nothing (ClassDef Nothing className classMembers))

-- Functions block frame. Stack grows downwards :)
-- [args]     Block frame does not contain class variables in case of method!!! 
emitFunc :: Function -> Label -> Maybe ClassName -> CompilerMonad ()
emitFunc func label className = case func of
    (Function retType args (Block _ stmts)) -> do
        let argsAsLocalVars = Map.fromList [(varName, (type_, FuncArg idx)) | ((varName, type_), idx) <- zip (argsToVarsTypes args) [0..]]
        emitFunc' retType argsAsLocalVars stmts False
    (Method retType args (Block _ stmts)) -> do
        let (Just className') = className
            argsAsLocalVars = Map.fromList [(varName, (type_, ClassMethodArg idx)) | ((varName, type_), idx) <- zip ((classSelfVariable, TClass Nothing className'):(argsToVarsTypes args)) [0..]]
        emitFunc' retType argsAsLocalVars stmts True 
    where 
        emitFunc' retType argsAsLocalVars stmts isMethod = do
        emitAssOp NEWLINE
        emitLabel =<< getNewLabelCustom labelInfoFnEntry
        emitAssOp $ if isMethod then METHODLABEL label else FNLABEL label
        emitProlog
        
        -- in Latte this will be empty for TopFuncDef as we have no global variables but might contain class variables for ClassMethods
        maybeClassVariables' <- gets variables -- set GC
        let variables' = Map.union argsAsLocalVars maybeClassVariables'
            scopeDeclaredVariables' = Set.fromList $ map fst (Map.toList argsAsLocalVars)
        
        emitFuncBlock argsAsLocalVars variables' scopeDeclaredVariables'
        emitEpilogue
        emitLabel =<< getNewLabelCustom labelInfoFnEnd

        modify (\state -> state {
            variables = maybeClassVariables' -- restore potential class variables
        })

        where 
            -- Memo previous DL and "init" current
            emitProlog = do
                emitAssOps [PUSH (Reg EBP), MOVL (Reg ESP) (Reg EBP)]
                let localsSpace = dWord * (countLocalVars stmts)
                when (localsSpace > 0) (emitAssOp $ SUBL (IntLit localsSpace) (Reg ESP))

            emitFuncBlock argsAsLocalVars variables' scopeDeclaredVariables' = do
                modify (\state -> state { blocksFrames = stackNew })
                pushBlockFrame [] 
                setLocalVarCount 0
                emitBlockWithMemo stmts variables' scopeDeclaredVariables' 

            emitEpilogue = do
                bf <- popBlockFrame
                emitFrameRefCntDecr bf
                when (isVoidType retType) (emitAssOps [LEAVE, RET]) 

            countLocalVars :: [StmtMeta] -> Int
            countLocalVars = sum . map go 
                where
                go (BStmt _ (Block _ stmts)) = countLocalVars stmts
                go (Decl _ _ items) = length items
                go (While _ _ body) = go body
                go (Cond _ _ thenStmt) = go thenStmt
                go (CondElse _ _ thenStmt elseStmt) = go thenStmt + go elseStmt
                go (For _ _ _ _ body) = 3 + go body     -- 3 because of ('hidden counter', tmp array, for loop var)
                go _ = 0

emitClass :: ClassName -> [ClassMember Position] -> CompilerMonad ()
emitClass className classMembers = do
    classMetadata <- getClassMetadata className
    let classMethods = filterClassMethods classMembers
        classFields = [(name, (type_, ClassVar idx)) | (name, (idx, type_, _)) <- OM.toList (fields classMetadata)]
        -- Map.Map :: FuncName FuncInfo
        classMethodToInfo = Map.fromList [(name, (Class source, type_)) | (name, (_, type_, source)) <- OM.toList (methods classMetadata)]

    globalFunctions <- gets functions
    setClassScope globalFunctions classMethodToInfo $ Map.fromList classFields
    emitMethods classMethods
    unsetClassScope globalFunctions
    GC.emitClassDestructor className classFields
    return ()

    where
        setClassScope :: FunctionsMap -> FunctionsMap -> VariablesMap -> CompilerMonad ()
        setClassScope globalFunctions classMethods classVariables = modify (\state -> state {
            variables = classVariables,
            functions = Map.union classMethods globalFunctions
        })

        unsetClassScope :: FunctionsMap -> CompilerMonad ()
        unsetClassScope globalFunctions = modify (\state -> state {
            functions = globalFunctions
        })

        emitMethods = mapM_ (\(ClassMethod _ (FuncDef _ type_ methodName args body)) ->
            emitFunc (Method type_ args body) (getMethodLabel className methodName) (Just className))


emitStmt :: StmtMeta -> CompilerMonad ()
emitStmt (Empty _) = return ()
emitStmt (SExp _ expr) = do
    (reg, type_) <- emitExprWithType expr
    GC.decrRefCntSafe type_ (Reg reg)
    return () 

emitStmt (VRet _) = do
    emitEndOfFuncBlockFrameRefCntDecr
    emitAssOps [LEAVE, RET]

emitStmt (Ret _ expr) = do
    (reg, type_) <- emitExprWithType expr
    emitAssOp $ PUSH (Reg reg)
    emitEndOfFuncBlockFrameRefCntDecr
    emitAssOps [POP (Reg EAX), LEAVE, RET]

emitStmt (BStmt _ (Block _ stmts)) = do
    variables' <- gets variables
    emitBlockWithMemo stmts variables' Set.empty

emitStmt (Cond _ expr stmt) = do
    labelThen <- getNewLabelCustom "cond_then"
    labelAfter <- getNewLabelCustom "cond_end"
    emitCond expr labelThen labelAfter
    emitLabel labelThen
    emitStmt stmt
    emitLabel labelAfter

emitStmt (CondElse _ expr stmtThen stmtElse) = do
    labelThen <- getNewLabelCustom "cond_then"
    labelElse <- getNewLabelCustom "cond_else"
    labelAfter <- getNewLabelCustom "cond_end"
    emitCond expr labelThen labelElse
    emitLabel labelThen
    emitStmt stmtThen
    emitAssOp $ JMP labelAfter
    emitLabel labelElse
    emitStmt stmtElse
    emitLabel labelAfter

emitStmt (While _ expr stmt) = do
    labelBody <- getNewLabelCustom "while_body"
    labelCond <- getNewLabelCustom "while_cond"
    labelAfter <- getNewLabelCustom whileEndLabel
    emitAssOp $ JMP labelCond
    emitLabel labelBody
    emitStmt stmt
    emitLabel labelCond
    emitCond expr labelBody labelAfter
    emitLabel labelAfter

emitStmt (For _ type_ varName arrExpr body) = do
    labelAfter <- getNewLabelCustom "for_loop_end"
    -- check if array not null
    let idxVar = Ident "__for_loop_idx"
        arrVar = Ident "__for_loop_arr"
    -- Evaluates array ONCE!!!
    (regArr, typeArr) <- emitExprWithType arrExpr
    -- Check if it's null or not
    emitCheckForNullArray regArr labelAfter
    
    -- declare && store temporary array
    varIdx <- addLocalVar arrVar typeArr
    varMemoryLoc <- getVarMemoryLoc varIdx
    emitAssOp $ MOVL (Reg regArr) varMemoryLoc

    let forEachBlock = BStmt Nothing $ Block Nothing [
            Decl Nothing (TInt Nothing) [Init Nothing idxVar (ELitInt Nothing 0)],              -- int idx = 0;
            While Nothing (ERel Nothing (EVar Nothing idxVar) (LTH Nothing) (EMember Nothing (EVar Nothing arrVar) (Ident "length"))) -- while (i <= arr.length)
            (BStmt Nothing $ Block Nothing [    
                Decl Nothing type_ [Init Nothing varName (EArrGet Nothing (EVar Nothing arrVar) (EVar Nothing idxVar))],
                body,
                Incr Nothing (EVar Nothing idxVar)
            ])
            ]
    emitStmt forEachBlock
    emitLabel labelAfter

    where
        emitCheckForNullArray reg labelAfter = emitAssOps [TEST (Reg reg) (Reg reg), JZ labelAfter]


emitStmt (Decl _ type_ items) = forM_ items emitDecl'
    where
        emitDecl' (Init _ varName expr) = do
            reg <- emitExpr expr
            varIdx <- addLocalVar varName type_
            varMemoryLoc <- getVarMemoryLoc varIdx
            emitAssOp $ MOVL (Reg reg) varMemoryLoc

        emitDecl' (NoInit _ varName) = do
            varIdx <- addLocalVar varName type_
            varMemoryLoc <- getVarMemoryLoc varIdx
            emitAssOp $ MOVL (IntLit 0) varMemoryLoc -- default value: 0 == null == ""

-- 3 cases of LValue. Each requires independend implementation because of their differences and ReferenceCount 
-- isLValue :: ExprMeta -> Bool
-- isLValue EVar {} = True
-- isLValue EArrGet {} = True
-- isLValue EMember {} = True
-- isLValue _ = False 
emitStmt (Ass _ (EVar _ varName) exprRValue) = do
    regRValue <- emitExpr exprRValue
    emitAssOp $ PUSH (Reg regRValue)                           -- memo RValue

    -- debug
    -- emitAssOps [
    --     CALL libPrintInt
    --     ]
    -- enddebug

    varType <- getVarType varName
    varIdx <- getVarIndex varName
    varMemoryLoc <- getVarMemoryLoc varIdx

    emitAssOp $ LEAL varMemoryLoc (Reg EAX)                     -- load address into EAX
    when (isComplexType varType) (do                            -- decr ref_cnt for old value
        emitAssOp $ PUSH (Reg EAX)
        GC.decrRefCntSafe varType varMemoryLoc
        emitAssOp $ POP (Reg EAX)                                 
        )
        
    emitAssOp $ POP (Reg ECX)                                  -- rValue
    emitAssOp $ MOVL (Reg ECX) (MemoryLoc EAX)

-- thanks to frontend we know that EMember will be an object not an array
emitStmt (Ass _ (EMember _ exprClass fieldName) exprRValue) = do
    regRValue <- emitExpr exprRValue
    emitAssOp $ SUBL (IntLit dWord) (Reg ESP)                  -- make space for the object's address
    emitAssOp $ PUSH (Reg regRValue)                           -- memo RValue
    (regClass, typeClass) <- emitExprWithType exprClass -- returns reference to the object 
    let (TClass _ className) = typeClass
    (fieldIdx, fieldType) <- getClassFieldInfo className fieldName

--    -- debug
--     emitAssOps [
--         PUSH (Reg regClass),
--         CALL libPrintInt,           -- object's address
--         POP (Reg EAX)
--         ]
--     -- enddebug

    emitAssOp $ MOVL (Reg EAX) (MemoryLocOffset ESP dWord) -- memorize object's address
    
    -- calculate address of the field
    emitAssOp $ LEAL (MemoryLocOffset EAX (getFieldMemoryOffset fieldIdx)) (Reg EAX)


    
    -- -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     CALL libPrintInt,           -- field's address
    --     POP (Reg EAX)
    --     ]
    -- -- enddebug

    -- maybe decr ref_cnt for currently stored object
    when (isComplexType fieldType) (do
        emitAssOp $ PUSH (Reg EAX)          -- memorize address of the field
        emitAssOp $ MOVL (MemoryLoc EAX) (Reg EAX)
        GC.decrRefCntSafe typeClass (Reg EAX)
        emitAssOp $ POP (Reg EAX)
        )

    -- -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     CALL libPrintInt,           -- field's address
    --     POP (Reg EAX)
    --     ]
    -- -- enddebug

    emitAssOp $ POP (Reg ECX)                                   -- get RValue

    -- -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     PUSH (Reg ECX),
    --     CALL libPrintInt,           -- RValue 
    --     POP (Reg ECX),
    --     POP (Reg EAX)
    --     ]
    -- -- enddebug

    emitAssOp $ MOVL (Reg ECX) (MemoryLoc EAX)                  -- load it: class.field = value;

    -- bump down inherited ref_cnt
    emitAssOp $ POP (Reg ECX)                                   -- get object's address
    
    -- -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     PUSH (Reg ECX),
    --     CALL libPrintInt,           -- object's address 
    --     POP (Reg ECX),
    --     POP (Reg EAX)
    --     ]
    -- -- enddebug

    -- -- debug
    -- emitAssOps [
    --     PUSH (Reg ECX),
    --     CALL libPrintInt,
    --     POP (Reg ECX)
    --     ]
    -- -- enddebug


    GC.decrRefCntSafe typeClass (Reg ECX)


emitStmt (Ass _ (EArrGet _ exprArr exprIdx) exprRValue) = do
    (regRValue, elementType) <- emitExprWithType exprRValue
    emitAssOp $ SUBL (IntLit dWord) (Reg ESP)                  -- (*) make space for the array's address
    emitAssOp $ PUSH (Reg regRValue)                           -- memo RValue
    regArr <- emitExpr exprArr   -- returns reference to the array
    emitAssOp $ MOVL (Reg EAX) (MemoryLocOffset ESP dWord)     -- (*) memorize array's address
    regIdx <- emitExpr exprIdx
    emitAssOp $ MOVL (Reg regIdx) (Reg ECX)
    emitAssOp $ INCL (Reg ECX)                                 -- +1 because of the array's layout in memory
    emitAssOp $ MOVL (MemoryLocOffset ESP dWord) (Reg EAX)     -- get array's address
    emitAssOp $ LEAL (MemoryLocOffset2 EAX ECX dWord) (Reg EAX)   -- memoryAddress of arr[idx]. Namely (arr)+idx

    when (isComplexType elementType) (do 
        emitAssOp $ PUSH (Reg EAX)                             -- memo address (arr+idx)
        emitAssOp $ MOVL (MemoryLoc EAX) (Reg EAX)
        GC.decrRefCntSafe elementType (Reg EAX)                -- maybe decr ref_cnt of old object
        emitAssOp $ POP (Reg EAX)                              -- restore address (arr+idx)
        -- emitAssOp $ MOVL (MemoryLoc EAX) (Reg EAX)          -- load where the reference points to (perhaps null)
        )

    emitAssOps [
        POP (Reg ECX),                                          -- restore RValue
        MOVL (Reg ECX) (MemoryLoc EAX),                         -- load new value into arr arr[idx] = value;
        POP (Reg EAX)                                           -- restore array's address
        ]
    
    labelAfter <- getNewLabelCustom "decr_ref_cnt_ass_earrget_end"
    GC.decrRefCnt EAX labelAfter
    emitLabel labelAfter

emitStmt Ass {} = undefined -- won't happen

emitStmt (Incr _ (EVar _ varName)) = do 
    varType <- getVarType varName
    varIdx <- getVarIndex varName
    varMemoryLoc <- getVarMemoryLoc varIdx
    emitAssOp $ INCL varMemoryLoc

emitStmt (Incr _ (EMember _ exprClass fieldName)) = do
    (reg, typeClass) <- emitExprWithType exprClass -- returns reference to the object
    let (TClass _ className) = typeClass
    (fieldIdx, _) <- getClassFieldInfo className fieldName

--    -- debug
--     emitAssOps [
--         PUSH (Reg regClass),
--         CALL libPrintInt,           -- object's address
--         POP (Reg EAX)
--         ]
--     -- enddebug

    -- emitAssOp $ LEAL (MemoryLocOffset EAX (getFieldMemoryOffset fieldIdx)) (Reg EAX) -- calculate address of the field
    emitAssOp $ INCL (MemoryLocOffset reg (getFieldMemoryOffset fieldIdx)) 

    -- -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     CALL libPrintInt,           -- field's address
    --     POP (Reg EAX)
    --     ]
    -- -- enddebug

    labelAfter <- getNewLabelCustom "decr_ref_cnt_ass_incr_end"
    GC.decrRefCnt reg labelAfter
    emitLabel labelAfter

emitStmt (Incr _ (EArrGet _ exprArr exprIdx)) = do
    regIdx <- emitExpr exprIdx
    emitAssOp $ PUSH (Reg regIdx)        -- memorize idx 
    regArr <- emitExpr exprArr    -- returns reference to the array
    emitAssOp $ MOVL (Reg EAX) (Reg EAX) -- EAX contains array's address
    emitAssOp $ POP (Reg ECX)            -- restore idx
    emitAssOp $ INCL (Reg ECX)                              -- +1 because of the array's layout in memory
    emitAssOp $ INCL (MemoryLocOffset2 EAX ECX dWord)       -- increment value under arr[idx]
    
    labelAfter <- getNewLabelCustom "decr_ref_cnt_ass_incr_end"
    GC.decrRefCnt EAX labelAfter
    emitLabel labelAfter

emitStmt (Incr _ _) = undefined

emitStmt (Decr _ (EVar _ varName)) = do 
    varType <- getVarType varName
    varIdx <- getVarIndex varName
    varMemoryLoc <- getVarMemoryLoc varIdx
    emitAssOp $ DECL varMemoryLoc

emitStmt (Decr _ (EMember _ exprClass fieldName)) = do
    (reg, typeClass) <- emitExprWithType exprClass -- returns reference to the object
    let (TClass _ className) = typeClass
    (fieldIdx, _) <- getClassFieldInfo className fieldName
    emitAssOp $ DECL (MemoryLocOffset reg (getFieldMemoryOffset fieldIdx)) 
    labelAfter <- getNewLabelCustom "decr_ref_cnt_ass_decr_end"
    GC.decrRefCnt reg labelAfter
    emitLabel labelAfter

emitStmt (Decr _ (EArrGet _ exprArr exprIdx)) = do
    regIdx <- emitExpr exprIdx
    emitAssOp $ PUSH (Reg regIdx)        -- memorize idx 
    regArr <- emitExpr exprArr    -- returns reference to the array
    emitAssOp $ MOVL (Reg EAX) (Reg EAX) -- EAX contains array's address
    emitAssOp $ POP (Reg ECX)            -- restore idx
    emitAssOp $ INCL (Reg ECX)                              -- +1 because of the array's layout in memory
    emitAssOp $ INCL (MemoryLocOffset2 EAX ECX dWord)       -- increment value under arr[idx]
    
    labelAfter <- getNewLabelCustom "decr_ref_cnt_ass_decr_end"
    GC.decrRefCnt EAX labelAfter
    emitLabel labelAfter

emitStmt (Decr _ _) = undefined


------------------------------- Expressions -------------------------------

emitExpr :: ExprMeta -> CompilerMonad Register
emitExpr expr = do
    (reg, _) <- emitExprWithType expr
    return reg 

emitExprWithType :: ExprMeta -> CompilerMonad (Register, TypeMeta) 
emitExprWithType (ELitTrue _) = do
  emitAssOp $ MOVL (IntLit 1) (Reg EAX)
  return (EAX, typeBool)

emitExprWithType (ELitFalse _) = do
  emitAssOp $ XORL (Reg EAX) (Reg EAX)
  return (EAX, typeBool)

emitExprWithType (ELitInt _ val) = do
    emitAssOp $ MOVL (IntLit (fromInteger val)) (Reg EAX)
    return (EAX, typeInt)

emitExprWithType (EString _ str) = do
    label <- getConstStrLabel str
    emitAssOp $ MOVL (Label label) (Reg EAX)
    emitAssOp $ ADDL (IntLit dWord) (Reg EAX) -- const string padding
    GC.incrRefCnt (Reg EAX)
    return (EAX, typeString)

emitExprWithType (Neg _ expr) = do
    reg <- emitExpr expr
    emitAssOp $ NEGL (Reg reg)
    return (reg, typeInt)

emitExprWithType (Not _ expr) = do
    reg <- emitExpr expr
    emitAssOps [XORL (IntLit 1) (Reg reg)]
    return (reg, typeBool)

emitExprWithType cond@(EAnd _ expr1 expr2) = do
    labelTrue <- getNewLabelCustom "and_set_true"
    labelFalse <- getNewLabelCustom "and_set_false"
    labelAfter <- getNewLabelCustom "and_set_after"

    emitCond cond labelTrue labelFalse
    emitLabel labelTrue
    emitAssOps [MOVL (IntLit 1) (Reg EAX), JMP labelAfter]
    emitLabel labelFalse
    emitAssOps [XORL (Reg EAX) (Reg EAX)]
    emitLabel labelAfter
    return (EAX, typeBool)

emitExprWithType cond@(EOr _ expr1 expr2) = do
    labelTrue <- getNewLabelCustom "or_set_true"
    labelFalse <- getNewLabelCustom "or_set_false"
    labelAfter <- getNewLabelCustom "or_set_after"

    emitCond cond labelTrue labelAfter
    emitLabel labelTrue
    emitAssOps [MOVL (IntLit 1) (Reg EAX), JMP labelAfter]
    emitLabel labelFalse
    emitAssOps [XORL (Reg EAX) (Reg EAX)]
    emitLabel labelAfter
    return (EAX, typeBool)

emitExprWithType (ERel _ expr1 relOp expr2) = do
    (reg1, type1) <- emitExprWithType expr1
    emitAssOp $ PUSH (Reg reg1) -- save result for later
    (reg2, type2) <- emitExprWithType expr2
    emitAssOp $ MOVL (Reg reg2) (Reg EAX)
    case (type1, type2) of
        (TStr _, TStr _) -> do
            emitAssOps [PUSH (Reg EAX), CALL "__streq"]
            stringBinOpPostDecrRefCnt
            emitAssOp $ ADDL (IntLit $ 2*dWord) (Reg ESP) -- caller epilogue 
            case relOp of
                NE _ -> emitAssOp $ XORL (IntLit 1) (Reg EAX) 
                _ -> return ()
            return (EAX, typeBool)
        -- TODO: optimize
        (TClass _ _, TClass _ _) -> do
            emitAssOp $ PUSH (Reg EAX)
                -- MOVL (Reg ESP) (Reg ECX),
                -- PUSH (Reg EAX),           
                -- CMPL (Reg EAX) (Reg ECX)  -- test (ECX - EAX)
                -- ]
            classBinOpPostDecrRefCnt type1
            emitAssOps [
                POP (Reg EAX),
                POP (Reg ECX)
                ]
            -- emitAssOp $ ADDL (IntLit $ 2*dWord) (Reg ESP) 
            -- emitAssOp $ TEST (Reg EAX) (Reg EAX)
            -- emitAssOp $ XORL (Reg ECX) (Reg EAX)
            labelEnd <- getNewLabelCustom "erel_end"
            labelFalse <- getNewLabelCustom "erel_true"

            emitAssOps [
                    CMPL (Reg ECX) (Reg EAX),
                    negJmp labelFalse,
                    MOVL (IntLit 1) (Reg EAX),
                    JMP labelEnd
                ]
            
            emitLabel labelFalse
            emitAssOp $ XORL (Reg EAX) (Reg EAX) -- set result comparison to true and proceed to end
            emitLabel labelEnd
            
            return (EAX, typeBool)
        _ -> do
            labelEnd <- getNewLabelCustom "erel_end"
            labelFalse <- getNewLabelCustom "erel_true"
            emitAssOps [
                POP (Reg ECX),
                CMPL (Reg EAX) (Reg ECX),  -- test (EAX - ECX)
                negJmp labelFalse,         -- jump to 'set to true' block if true
                MOVL (IntLit 1) (Reg EAX), -- set result comparison to false and jmp to end
                JMP labelEnd
                ]
            emitLabel labelFalse
            emitAssOp $ XORL (Reg EAX) (Reg EAX) -- set result comparison to true and proceed to end
            emitLabel labelEnd
            return (EAX, typeBool)

        where
            negJmp = case relOp of
                NE _ -> JE
                EQU _ -> JNE
                LE _ -> JG
                LTH _ -> JGE
                GE _ -> JL
                GTH _ -> JLE
            
emitExprWithType (EMul _ expr1 mulOp expr2) = do
    if canLoadSimpleExpr expr1 expr2
        then
            void (emitLoadSimpleExpr expr1 expr2)
        else (do 
            reg2 <- emitExpr expr2      -- result in EAX
            emitAssOp $ PUSH (Reg reg2)
            reg1 <- emitExpr expr1   -- res in EAX
            -- emitAssOps [MOVL (Reg reg2) (Reg ECX), POP (Reg EAX)]
            emitAssOp $ POP (Reg ECX))
        
    -- arg1 in EAX, arg2 in EDX 
    case mulOp of
        (Times _) -> do
            emitAssOp $ IMUL (Reg ECX) (Reg EAX)
            return (EAX, typeInt)       
        (Div _) -> do
            emitAssOps [CLTD, IDIVL (Reg ECX)]
            return (EAX, typeInt)
        (Mod _) -> do
            emitAssOps [CLTD, IDIVL (Reg ECX)]
            emitAssOp $ MOVL (Reg EDX) (Reg EAX)
            return (EAX, typeInt)

emitExprWithType (EAdd _ expr1 addOp expr2) = do
    -- loads expr1, expr2 into (EAX, ECX)
    (type1, type2) <- if canLoadSimpleExpr expr1 expr2
        then
            emitLoadSimpleExpr expr1 expr2
        else (do
            (reg1, type1) <- emitExprWithType expr1
            emitAssOp $ PUSH (Reg reg1)
            (reg2, type2) <- emitExprWithType expr2
            emitAssOps [MOVL (Reg reg2) (Reg ECX), POP (Reg EAX)]
            -- (reg2, type2) <- emitExprWithType expr2
            -- emitAssOp $ PUSH (Reg reg2)
            -- (reg1, type1) <- emitExprWithType expr1
            -- emitAssOps [POP (Reg ECX)]
            return (type1, type2))

    case addOp of
        (Plus _) ->
            case (type1, type2) of
                (TInt _, TInt _) -> do
                    emitAssOp $ ADDL (Reg ECX) (Reg EAX)
                    return (EAX, typeInt)
                (TStr _, TStr _) -> do
                    -- note that for s1+s2 we are passing (s2,s1) thus reversed ordered
                    emitAssOps [PUSH (Reg EAX), PUSH (Reg ECX), CALL "__rstrconcat"]
                    stringBinOpPostDecrRefCnt
                    emitAssOp $ ADDL (IntLit (2*dWord)) (Reg ESP)
                    GC.setDestructor EAX stringDestructorLabel
                    GC.initRefCnt EAX
                    return (EAX, typeString)
                _ -> undefined
        (Minus _) -> do
            emitAssOp $ SUBL (Reg ECX) (Reg EAX)
            return (EAX, typeInt)

emitExprWithType (EVar _ varName) = do
    varIdx <- getVarIndex varName
    varType <- getVarType varName
    varMemoryLoc <- getVarMemoryLoc varIdx 

    emitAssOp $ MOVL varMemoryLoc (Reg EAX)       -- calculate actual value of the variable 
    emitAssOp $ PUSH (Reg EAX)
    GC.incrRefCntSafe varType varMemoryLoc
    emitAssOp $ POP (Reg EAX)
    return (EAX, varType)

emitExprWithType (EApp _ funcName argsExprs) = do
    -- we want to pass args in reversed order, so that first arg is just before EBP
    -- arg3
    -- arg2
    -- arg1
    -- return address
    -- EBP

    forM_ (reverse argsExprs) emitArg

    (funcScope, (retType, _)) <- unpackFunc funcName
    let numberOfArgs = length argsExprs
    blocksFrames' <- gets blocksFrames
    modify (\state -> state {blocksFrames = stackNew})
    emitCall (funcScope, numberOfArgs)
    modify (\state -> state {blocksFrames = blocksFrames'})
    return (EAX, retType)

    where
        unpackFunc :: VarName -> CompilerMonad (FuncScope, FuncType)
        unpackFunc funcName = do
            functions' <- gets functions
            -- class methods shadow global functions. Namely, they are added to 'functions' when in scope
            return $ (Map.!) functions' funcName

        emitCall :: (FuncScope, Int) -> CompilerMonad ()
        emitCall (Global, nArgs) = do
            emitAssOp $ CALL (show funcName)
            -- adjust ESP to fit args
            emitCleanStackDecrRefCnt
            unless (nArgs == 0) (emitAssOp $ ADDL (IntLit (dWord * nArgs)) (Reg ESP))
        emitCall (Class className, nArgs) = do
            emitAssOps [
                MOVL (MemoryLocOffset EBP (2*dWord)) (Reg EAX), -- load 'this' into EAX, +2*dWord because: ('this', return address, EBP)
                PUSH (Reg EAX),                                 -- push 'this' as argument
                CALL (getMethodLabel className funcName)
                ]
            emitCleanStackDecrRefCnt 
            emitAssOp $ ADDL (IntLit (dWord * nArgs + dWord)) (Reg ESP) -- 'pop' args from the stack

        emitCleanStackDecrRefCnt = do
            complexArgsInfo <- getComplexArgsInfo
            emitAssOp $ PUSH (Reg EAX)
            forM_ complexArgsInfo (\(type_, idx) -> GC.decrRefCntSafe type_ (MemoryLocOffset ESP (dWord * (idx + 1))))
            emitAssOp $ POP (Reg EAX)
            return ()

        getComplexArgsInfo :: CompilerMonad [(TypeMeta, Int)]
        getComplexArgsInfo = do
            functions' <- gets functions
            let (_, (_, argsTypes)) = (Map.!) functions' funcName
            return $ catMaybes $ map (\(type_, idx) -> case type_ of
                (TClass _ _) -> Just (type_, idx)
                (TArray _ _) -> Just (type_, idx)
                (TStr _) -> Just (type_, idx)
                _ -> Nothing
                ) (zip argsTypes [0..])


-------------------------------- Objects --------------------------------
emitExprWithType (ECast _ className) = do
    emitAssOp $ XORL (Reg EAX) (Reg EAX)
    return (EAX, TClass Nothing className)

-- object's layout in memory
-- [dtor, ref_cnt, vTable, fields...]
--                 ^pointer to object
-- type \in {className}
emitExprWithType (ENewObject _ type_@(TClass _ className)) = do
    classMetadata <- getClassMetadata className 
    let fields' = fields classMetadata
        numOfFields = OM.size fields'
        numOfMethods = OM.size (methods classMetadata)
    -- each field is either of simpleType or it's a reference to complexType.
    -- thus each field requires 32bits
    -- void* calloc (size_t num, size_t size);    
    -- allocated memory is filled with bytes of values 0
    emitAssOps [PUSH (IntLit dWord), PUSH (IntLit (numOfFields + classMetadataSize))] 
    emitAssOp $ CALL "calloc"   -- EAX contains pointer to allocated memory
    emitCallerEpilogue 2
    
    -- Garbage collector dependencies
    addBaseMetadataOffset EAX
    GC.initRefCnt EAX
    GC.setDestructor EAX (getClassDtorLabel className)
  -- -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     CALL libPrintInt, -- objetc's address
    --     POP (Reg EAX)
    --     ]
  -- -- end debug
    unless (numOfMethods == 0) (emitAssOp $ MOVL (Label $ getVTableLabel className) (MemoryLoc EAX))
    return (EAX, type_) 

emitExprWithType (ENewObject _ _) = undefined  -- won't happen thanks to frontend


-- array's layout in memory:
-- [dtor, ref_cnt, array_length, first_element, second_element, ...]
--                 ^pointer to array 
-- type \in {className, simpleType}
emitExprWithType (ENewArray _ type_ sizeExpr) = do
    reg <- emitExpr sizeExpr
    emitAssOp $ PUSH (Reg reg)                            -- save actual length for later
    emitAssOp $ ADDL (IntLit arrayMetadataSize) (Reg reg) 
    emitAssOps [PUSH (IntLit dWord), PUSH (Reg reg)] 
    emitAssOp $ CALL "calloc"                             -- EAX stores address of reserved memory
    emitCallerEpilogue 2

    -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     CALL libPrintInt,
    --     POP (Reg EAX)
    --     ]
    -- enddebug
     
    addBaseMetadataOffset EAX
    setDestructor' EAX
    GC.initRefCnt EAX
    emitAssOp $ POP (Reg ECX)                  -- restore actual length
    emitAssOp $ MOVL (Reg ECX) (MemoryLoc EAX) -- store length "on top of the array"
    return (EAX, TArray Nothing type_)

    where
        setDestructor' reg = case type_ of
            (TClass _ _) -> GC.setDestructor reg arrayComplexDestructorLabel
            (TStr _) -> GC.setDestructor reg arrayComplexDestructorLabel
            _ -> GC.setDestructor reg arraySimpleDestructorLabel


-- TODO reverse order of calculation
emitExprWithType (EArrGet _ arrExpr idxExpr) = do
    (regArr, arrType) <- emitExprWithType arrExpr
    let (TArray _ elementType) = arrType
    emitAssOp $ PUSH (Reg regArr)
    regIdx <- emitExpr idxExpr
    emitAssOps [
        MOVL (Reg regIdx) (Reg ECX), -- idx
        INCL (Reg ECX),                                  -- +1: arr.length
        MOVL (MemoryLoc ESP) (Reg EAX),                  -- array memory location points to arr.length
        LEAL (MemoryLocOffset2 EAX ECX dWord) (Reg EAX)  -- memoryAddress of arr[idx]
        ]
    when (isComplexType elementType) (do
        emitAssOp $ PUSH (Reg EAX)
        emitAssOp $ MOVL (MemoryLoc EAX) (Reg EAX)
        GC.incrRefCnt (Reg EAX)
        emitAssOp $ POP (Reg EAX)
        )
    emitAssOp $ MOVL (MemoryLoc EAX) (Reg EAX)           -- calculate actual value of arr[idx]
    emitAssOp $ POP (Reg ECX)
    emitAssOp $ PUSH (Reg EAX)
    
    labelAfter <- getNewLabelCustom "decr_ref_cnt_earrget_end"
    GC.decrRefCnt ECX labelAfter
    emitLabel labelAfter

    emitAssOp $ POP (Reg EAX)
    -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     CALL libPrintInt,
    --     POP (Reg EAX)
    --     ]
    -- enddebug

    return (EAX, elementType)
    

emitExprWithType (EMember _ exprObject fieldName) = do
    (reg, objectType) <- emitExprWithType exprObject
    emitAssOp $ PUSH (Reg reg) -- memorize objects so that its counter can be decremented after access

     -- debug
    -- emitAssOps [
    --     PUSH (Reg reg),
    --     CALL libPrintInt,
    --     POP (Reg reg)
    --     ]
            -- enddebug

    (_, fieldType) <- case objectType of
        (TArray _ _) -> do
            -- emitAssOp $ CMPL (Reg reg) (IntLit 0)
            -- array has only attribute .length
            emitAssOp $ MOVL (MemoryLoc reg) (Reg EAX)
            -- emitAssOps [
            --     PUSH (Reg EAX),
            --     CALL libPrintInt,
            --     POP (Reg EAX)
            --     ]
            return (EAX, typeInt)

        (TClass _ className) -> do
            (fieldIdx, fieldType) <- getClassFieldInfo className fieldName
            -- debug
            -- emitAssOps [
            --     PUSH (Reg EAX),
            --     CALL libPrintInt,
            --     POP (Reg EAX)
            --     ]
            -- enddebug
            
            emitAssOp $ MOVL (MemoryLocOffset EAX (getFieldMemoryOffset fieldIdx)) (Reg EAX)
            -- debug
            -- emitAssOps [
            --     PUSH (Reg EAX),
            --     CALL libPrintInt,
            --     POP (Reg EAX)
            --     ]
            -- enddebug

            when (isComplexType fieldType) (do
                emitAssOp $ PUSH (Reg EAX) 
                GC.incrRefCnt (Reg EAX)
                emitAssOp $ POP (Reg EAX))
            -- debug
            -- emitAssOps [
            --     PUSH (Reg EAX),
            --     CALL libPrintInt,
            --     POP (Reg EAX)
            --     ]
            -- enddebug
            return (EAX, fieldType)
        _ -> undefined  -- won't happen thanks to frontend
    
    emitAssOps [
        POP (Reg ECX), -- get object's address
        PUSH (Reg EAX) -- memorize field that was accessed
    -- debug
    --     PUSH (Reg ECX), -- memorize field that was accessed
    --     PUSH (IntLit 1000),
    --     CALL libPrintInt,
    --     POP (Reg ECX)
    --     POP (Reg ECX)
        ]
    -- enddebug
    labelAfter <- getNewLabelCustom "decr_ref_cnt_emember_end"
    GC.decrRefCnt ECX labelAfter             
    emitLabel labelAfter
    emitAssOp $ POP (Reg EAX)

    -- debug
    -- emitAssOps [
    --     PUSH (Reg EAX),
    --     CALL libPrintInt,
    --     POP (Reg EAX)
    --     ]
    -- enddebug

    return (EAX, fieldType)

emitExprWithType (EMemberCall _ exprObject methodName argsExprs) = do
    forM_ (reverse argsExprs) emitArg 
    
    (reg, objectType) <- emitExprWithType exprObject
    let (TClass _ className) = objectType -- safe thanks to frontend
    (idx, (retType, argsTypes)) <- getMethodInfo className
    
    emitCallerPrologue reg idx
    emitCall argsTypes
    
    return (EAX, retType)

    where
        getMethodInfo className = do
            classMetadata <- getClassMetadata className
            let (idx, (retType, argsTypes), _) = (OM.!) (methods classMetadata) methodName
            return (idx, (retType, argsTypes))

        emitCallerPrologue reg idx = emitAssOps [
            -- reg stores reference to the object
            PUSH (Reg reg),      -- push "this" (reference to the object) on stack, other args are already there, thus it will be first one
            MOVL (MemoryLoc reg) (Reg reg),    -- load "this" from memory -> load its vTable 
            MOVL (MemoryLocOffset reg (dWord * idx)) (Reg EAX) -- load function from its position in vTable
            ]

        emitCall argsTypes = do
            emitAssOp $ CALLREG EAX
            -- remove method's args from stack. At least "self" is to be removed
            emitCleanStackDecrRefCnt argsTypes
            emitAssOp $ ADDL (IntLit (dWord * (length argsExprs) + dWord)) (Reg ESP)


        emitCleanStackDecrRefCnt argsTypes = do
            let complexArgsInfo = getComplexArgsInfo argsTypes
            emitAssOp $ PUSH (Reg EAX)
            forM_ complexArgsInfo (\(type_, idx) -> GC.decrRefCntSafe type_ (MemoryLocOffset ESP (dWord * (idx + 2))))
            GC.decrRefCntSafe typeClassJoker (MemoryLocOffset ESP dWord)
            emitAssOp $ POP (Reg EAX)
            return ()
        
        getComplexArgsInfo :: [VarType] -> [(TypeMeta, Int)]
        getComplexArgsInfo argsTypes = catMaybes $ map (\(type_, idx) -> case type_ of
                (TClass _ _) -> Just (type_, idx)
                (TArray _ _) -> Just (type_, idx)
                (TStr _) -> Just (type_, idx)
                _ -> Nothing
                ) (zip argsTypes [0..])



---------------------------- Cond Expr evaluation ----------------------------
emitCond :: ExprMeta -> Label -> Label -> CompilerMonad ()
emitCond (ELitTrue _) lTrue _ = emitAssOp $ JMP lTrue
emitCond (ELitFalse _) _ lFalse = emitAssOp $ JMP lFalse
emitCond (Not _ expr) lTrue lFalse = emitCond expr lFalse lTrue
emitCond (EAnd _ expr1 expr2) lTrue lFalse = do
    lMid <- getNewLabelCustom "and_right_expr"
    emitCond expr1 lMid lFalse
    emitLabel lMid
    emitCond expr2 lTrue lFalse

emitCond (EOr _ expr1 expr2) lTrue lFalse = do
    lMid <- getNewLabelCustom "or_right_expr"
    emitCond expr1 lTrue lMid
    emitLabel lMid
    emitCond expr2 lTrue lFalse   

emitCond (ERel _ expr1 relOp expr2) lTrue lFalse = do
    (reg1, type1) <- emitExprWithType expr1
    emitAssOp $ PUSH (Reg reg1)                    -- save result for later
    (reg2, type2) <- emitExprWithType expr2        -- result in EAX
    emitAssOp $ MOVL (Reg reg2) (Reg EAX)
    case (type1, type2) of
        (TStr _, TStr _) -> do
            -- __streq returns 1 if two strings are equal
            emitAssOps [PUSH (Reg EAX), CALL "__streq"]
            stringBinOpPostDecrRefCnt
            emitAssOp $ ADDL (IntLit $ 2*dWord) (Reg ESP)
            emitAssOp $ TEST (Reg EAX) (Reg EAX) -- like bitwise AND
            case relOp of
                NE _ -> emitAssOps [JZ lTrue, JMP lFalse]
                _ -> emitAssOps [JZ lFalse, JMP lTrue]
        -- TODO optimize
        (TClass _ _, TClass _ _) -> do
            emitAssOp $ PUSH (Reg EAX)
            classBinOpPostDecrRefCnt type1
            emitAssOps [
                POP (Reg EAX),
                POP (Reg ECX)
                ]
            emitAssOp $ CMPL (Reg EAX) (Reg ECX)
            case relOp of
                NE _ -> emitAssOps [JZ lFalse, JMP lTrue]
                _ -> emitAssOps [JZ lTrue, JMP lFalse]
        _ -> do
            emitAssOps [
                POP (Reg ECX),
                CMPL (Reg EAX) (Reg ECX),  -- test (ECX - EAX)
                negJmp lFalse,     
                JMP lTrue
                ]
        where
            -- Note: this trick allow us to avoid generating dummy code like this:
            -- JE label1
            -- JMP label2
            -- label1:
            -- ...
          -- Basically we avoid generating many unnecessary jumps
            -- Generated code looks like this:
            -- JNE label2
            -- JMP label1
            -- label1
            -- To fix it, once the code is generated, jumps of this form are removed
            -- It might generate one unnecessary jump at the beginning of expression,
            -- but later in, deeper in complex expression it will not generate it, 
            -- because the "result label" will be far away
            -- Namely, for instance for (a || (complex expr...))
            -- it will generate sth like
            --   cmpl %eax, %ecx
            --   jle .or_right_expr_0
            --   jmp .and_right_expr_0
            -- .or_right_expr_0:
            negJmp = case relOp of
                NE _ -> JE
                EQU _ -> JNE
                LE _ -> JG
                LTH _ -> JGE
                GE _ -> JL
                GTH _ -> JLE

emitCond expr lTrue lFalse = do
    reg <- emitExpr expr
    emitAssOps [
        TEST (Reg reg) (Reg reg),
        JZ lFalse,
        JMP lTrue
        ]


---------------------------- Misc -----------------------------

-- emits stmts as block with given variables and scopeDeclaredVariables and then restores them
-- fresh block frame is put
-- e.g. declaring variables that is already declared won't be valid
-- but on leave (other then return) ref_cnt updates will be performed only for newly declared vars ~ fresh block frame 
emitBlockWithMemo :: [StmtMeta] -> VariablesMap -> Set.Set VarName -> CompilerMonad ()
emitBlockWithMemo stmts variables' scopeDeclaredVariables' = do
    blocksFrames' <- gets blocksFrames
    modify (\state -> state {
        variables = variables',
        scopeDeclaredVariables = scopeDeclaredVariables',
        blocksFrames = stackPush blocksFrames' []
    })
    forM_ stmts emitStmt
    blockFrame <- peekBlockFrame
    emitFrameRefCntDecr blockFrame
    modify (\state -> state {
        variables = variables',
        scopeDeclaredVariables = scopeDeclaredVariables',
        blocksFrames = blocksFrames'
    })

emitFrameRefCntDecr :: BlockFrame -> CompilerMonad ()
emitFrameRefCntDecr = do
    mapM_ (\(type_, varIdx) -> 
        when (isComplexType type_) (do
            varMemoryLoc <- getVarMemoryLoc varIdx
            GC.decrRefCntSafe type_ varMemoryLoc))

-- rewinds all the blocks for given functions
-- {
--    int a;
-- }
-- int x;
--     {
--         int s;
--         {
--             int x;
--             return;
--         }
--     }
-- shoulde decr_cnt for x,s,x', but not for a;
emitEndOfFuncBlockFrameRefCntDecr :: CompilerMonad ()
emitEndOfFuncBlockFrameRefCntDecr = do
    blocksFrames' <- gets blocksFrames
    let Stack bfs = blocksFrames'
    forM_ bfs emitFrameRefCntDecr
    -- after this operation stack becomes empty, and the return is called,
    -- the caller is responsible of memorizing his stack from the moment of function call

-- bumps down ref_cnt for two values on top of the stack
classBinOpPostDecrRefCnt :: VarType -> CompilerMonad ()
classBinOpPostDecrRefCnt type_ = do
    GC.decrRefCntSafe type_ (MemoryLoc ESP)
    GC.decrRefCntSafe type_ (MemoryLocOffset ESP dWord)

-- bumps down ref_cnt for two values on top of the stack preserving EAX
stringBinOpPostDecrRefCnt :: CompilerMonad ()
stringBinOpPostDecrRefCnt = do
    emitAssOp $ PUSH (Reg EAX) -- memo result of comparison
    GC.decrRefCntSafe typeString (MemoryLocOffset ESP dWord)
    GC.decrRefCntSafe typeString (MemoryLocOffset ESP (2*dWord))
    emitAssOp $ POP (Reg EAX) -- restore result

emitArg :: ExprMeta -> CompilerMonad ()
emitArg expr = do
    reg <- emitExpr expr
    emitAssOp $ PUSH (Reg reg)

emitCallerEpilogue :: Int -> CompilerMonad ()
emitCallerEpilogue numOfArgs = emitAssOp $ ADDL (IntLit (dWord * numOfArgs)) (Reg ESP)

addBaseMetadataOffset :: Register -> CompilerMonad ()
addBaseMetadataOffset reg = emitAssOp $ ADDL (IntLit (baseMetadataSize * dWord)) (Reg reg)

emitReadStringLibFunc :: CompilerMonad ()
emitReadStringLibFunc = do
    emitAssOp NEWLINE
    emitLabel =<< getNewLabelCustom labelInfoFnEntry
    emitAssOp $ FNLABEL "readString"
    emitAssOp $ CALL "__read_string"
    GC.initRefCnt EAX
    GC.setDestructor EAX stringDestructorLabel
    emitAssOp RET
    emitLabel =<< getNewLabelCustom labelInfoFnEnd



canLoadSimpleExpr expr1 expr2 = case (expr1, expr2) of
    (EVar _ _, EVar _ _) -> True
    (EVar _ _, ELitInt _ _) -> True
    (ELitInt _ _, EVar _ _) -> True
    _ -> False

-- loads results into (EAX, EDX) respectively
emitLoadSimpleExpr :: ExprMeta -> ExprMeta -> CompilerMonad (TypeMeta, TypeMeta)
emitLoadSimpleExpr (EVar _ varName) (ELitInt _ v) = do
    varMemoryLoc <- getVarMemoryLoc =<< getVarIndex varName
    emitAssOps [
        MOVL varMemoryLoc (Reg EAX),
        MOVL (IntLit $ fromInteger v) (Reg ECX)
        ]
    return (typeInt, typeInt)

emitLoadSimpleExpr (ELitInt _ v) (EVar _ varName) = do
    varMemoryLoc <- getVarMemoryLoc =<< getVarIndex varName
    emitAssOps [
        MOVL (IntLit $ fromInteger v) (Reg EAX),
        MOVL varMemoryLoc (Reg ECX)
        ]
    return (typeInt, typeInt)

-- TODO incr count for strings...
emitLoadSimpleExpr (EVar _ varName1) (EVar _ varName2) = do
    type_ <- getVarType varName1
    if isComplexType type_ then (do
        varMemoryLoc1 <- getVarMemoryLocWith ECX =<< getVarIndex varName1
        emitAssOp $ MOVL varMemoryLoc1 (Reg EAX) -- Load value
        emitAssOp $ PUSH (Reg EAX) -- memo val1
        GC.incrRefCntSafe type_ varMemoryLoc1

        varMemoryLoc2 <- getVarMemoryLocWith ECX =<< getVarIndex varName2
        emitAssOp $ MOVL varMemoryLoc2 (Reg EAX) -- Load value
        emitAssOp $ PUSH (Reg EAX) -- memo val2
        GC.incrRefCntSafe type_ varMemoryLoc2

        emitAssOp $ POP (Reg ECX) -- restore val2
        emitAssOp $ POP (Reg EAX) -- restore val1
        return (type_, type_)) -- namely string
    else (do
        varMemoryLoc1 <- getVarMemoryLocWith EAX =<< getVarIndex varName1
        varMemoryLoc2 <- getVarMemoryLocWith ECX =<< getVarIndex varName2
        emitAssOps [
            MOVL varMemoryLoc1 (Reg EAX),
            MOVL varMemoryLoc2 (Reg ECX)
            ]
        return (type_, type_))
    
-- ELitInt ELitInt is already optimized by frontend
emitLoadsimpleExpr _ _ = undefined
