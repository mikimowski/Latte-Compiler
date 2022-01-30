{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import AbsLatte

import TypeCheckerUtils
import StaticError
import Utils
import Common
import OrderedMap
import ProgramSimplifier

------------------------  Aggregators -----------------------
registerFunction :: TopDefMeta -> TypeCheckerMonad ()
registerFunction (TopFuncDef _ funcDef@(FuncDef position retType funcName args body)) = do
    checkForRedeclaration funcName
    registerFunction'
    return () 
   
    where
        checkForRedeclaration :: Ident -> TypeCheckerMonad () 
        checkForRedeclaration funcName = do
            functions' <- gets functions
            when (Map.member funcName functions') $ throwError (StaticError position (FunctionRedeclaration funcName))

        registerFunction' = do
            functions' <- gets functions
            modify (\env -> env {
                functions = Map.insert funcName (buildFuncType funcDef) functions'
            })
registerFunction _ = return ()

------------------------  Block -----------------------
typeCheckBlock :: BlockMeta -> TypeCheckerMonad ()
typeCheckBlock (Block _ stmts) = do
    let stateModifyAction state = state {
        blockDefinedVariables = Set.empty
    }
    runLocally stateModifyAction (mapM_ typeCheckStmt stmts)

typeCheckBlockWithCurrentEnv :: BlockMeta -> TypeCheckerMonad ()
typeCheckBlockWithCurrentEnv (Block _ stmts) = do
    runLocally id (mapM_ typeCheckStmt stmts)

------------------------  TopDefinitions -----------------------
typeCheckTopDef :: TopDefMeta -> TypeCheckerMonad ()
typeCheckTopDef (TopFuncDef _ funcDef) = typeCheckFuncDef funcDef
typeCheckTopDef (TopClassDef _ classDef) = typeCheckClassDef classDef

----------------------- FuncDef -----------------------
typeCheckFuncDef :: FuncDefMeta -> TypeCheckerMonad ()
typeCheckFuncDef (FuncDef position retType funcName args body) = do
    checkForVoidArgs
    checkForUndefinedTypesOfArgs
    checkForDuplicateArgs
    variables' <- gets variables 
    let argsTypes = map argToVarType args
    let stateModifyAction state = state {
        variables = Map.union (Map.fromList argsTypes) variables', 
        blockDefinedVariables = Set.fromList (map fst argsTypes),
        validReturnType = retType
    }
    runLocally stateModifyAction (typeCheckBlockWithCurrentEnv body)
    return ()

    where
        checkForVoidArgs = do
            let maybeVoidArg = find (\(Arg _ argType _) -> case argType of
                    (TVoid _) -> True
                    (TArray _ (TVoid _)) -> True
                    _ -> False) args 
            case maybeVoidArg of
                Nothing -> return ()
                Just (Arg position' type_ argName) -> throwError (StaticError position' (VoidArgumentType argName type_ funcName))                

        checkForUndefinedTypesOfArgs = do
            forM_ args (\(Arg position' type_ argName) -> unlessM (isTypeDefined type_) (throwError (StaticError position' (TypeNotDeclared type_))))

        checkForDuplicateArgs = do
            let argsNamesCount = count $ map (\(Arg _ _ argName) -> argName) args
            forM_ argsNamesCount (\(argName, count) -> when (count > 1) $ throwError (StaticError position (DuplicateFunctionArgument argName funcName)))


------------------------ ClassDef -----------------------
typeCheckClassDef :: ClassDefMeta -> TypeCheckerMonad ()
typeCheckClassDef (ClassExtDef position className _ classBlock) = typeCheckClassDef (ClassDef position className classBlock)
typeCheckClassDef (ClassDef classPosition className classMembers) = do
    let classFieldsDefs = filterClassFields classMembers
        classMethodsDefs = filterClassMethods classMembers
    checkForDuplicateFields classFieldsDefs
    forM_ classFieldsDefs (\(ClassField position type_ fieldName) -> do
        unlessM (isTypeDefined type_) (throwError (StaticError position (TypeNotDeclared type_)))
        when (isVoidType type_) (throwError (StaticError position (InvalidType type_)))
        )
    checkForDuplicateMethods classMethodsDefs
    checkForSelfKeywordOverride classFieldsDefs classMethodsDefs

    classMetadata <- getClassMetadata className Nothing
    let classMethods = Map.fromList [(methodName, retType) | (methodName, (_, retType, _)) <- OrderedMap.toList (methods classMetadata)]
    let classFields = Map.fromList [(fieldName, fieldType) | (fieldName, (_, fieldType, _)) <- OrderedMap.toList (fields classMetadata)]
    functions' <- gets functions

    let stateModifyAction state = state {
        -- enables "self.method1()"
        variables = Map.union classFields (Map.fromList [(classSelfVariable, TClass Nothing className)]),
        -- first map's elements are prefered in case of duplicates
        -- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html#g:8
        functions = Map.union classMethods functions'
    }
    let classFuncDefs = map (\(ClassMethod position funcDef) -> funcDef) (filterClassMethods classMembers)
    runLocally stateModifyAction (forM_ classFuncDefs typeCheckFuncDef)

    where
        checkForSelfKeywordOverride classFieldsDefs classMethodsDefs = do
            when (className == classSelfVariable) (throwError (StaticError classPosition (SelfClassKeyword "class name")))
            forM_ classFieldsDefs (\(ClassField position _ fieldName) -> 
                when (classSelfVariable == fieldName) (throwError (StaticError position (SelfClassKeywordOverride "field"))))
            forM_ classMethodsDefs (\(ClassMethod _ (FuncDef position _ methodName args (Block _ stmts))) -> do
                when (classSelfVariable == methodName) (throwError (StaticError position (SelfClassKeywordOverride "method")))
                forM_ args (\(Arg argPosition _ argName) -> 
                    when (classSelfVariable == argName) (throwError (StaticError argPosition (SelfClassKeywordOverride "argument"))))
                checkBody stmts)

            where
                checkBody [] = return ()
                checkBody (x:xs) = do
                    case x of 
                       (Decl _ _ items) -> do
                           forM_ items (\case
                               (NoInit position varName) -> when (classSelfVariable == varName) (throwError (StaticError position (SelfClassKeywordOverride "variable"))) 
                               (Init position varName _) -> when (classSelfVariable == varName) (throwError (StaticError position (SelfClassKeywordOverride "variable")))) 
                       (Ass _ (EVar position varName) _) -> when (classSelfVariable == varName) (throwError (StaticError position SelfClassKeywordAssignment))
                       (For position _ varName _ _) -> when (classSelfVariable == varName) (throwError (StaticError position SelfClassKeywordAssignment)) 
                       _ -> return ()
                    checkBody xs

        checkForDuplicateFields classFieldsDefs = do
            let fieldsNamesCount = count $ map (\(ClassField _ _ fieldName) -> fieldName) classFieldsDefs
            forM_ fieldsNamesCount (\(fieldName, count) -> when (count > 1) $ throwError (StaticError Nothing (DuplicateAttribute fieldName className)))
        
        checkForDuplicateMethods classMethodsDefs = do
            let methodsNamesCount = count $ map (\(ClassMethod _ (FuncDef _ _ methodName _ _)) -> methodName) classMethodsDefs
            forM_ methodsNamesCount (\(methodName, count) -> when (count > 1) $ throwError (StaticError Nothing (DuplicateMethod methodName className)))


------------------------ Statements ------------------------
typeCheckStmt :: StmtMeta -> TypeCheckerMonad ()
typeCheckStmt (Empty _) = return ()
typeCheckStmt (BStmt _ block) = typeCheckBlock block
typeCheckStmt (SExp _ expr) = void (typeCheckExpr expr)
typeCheckStmt (Decl position type_ items) = do
    unlessM (isTypeDefined type_) (throwError (StaticError position (TypeNotDeclared type_)))
    when (isVoidType type_) (throwError (StaticError position (InvalidTypeVariableDeclaration type_))) 
    checkForVoidArray type_
    forM_ items typeCheckDecl

    where
        typeCheckDecl (NoInit _ varName) = declareVar varName type_
        typeCheckDecl (Init _ varName expr) = do
            exprType <- typeCheckExpr expr
            unlessM (isSubtypeOf exprType type_) (throwError (StaticError position (InvalidTypeInitialization exprType type_)))
            declareVar varName type_

typeCheckStmt (Ass position lValExpr rValExpr) = do
    unless (isLValue lValExpr) (throwError (StaticError position InvalidLValue))
    whenM (isArrayMemberAccess lValExpr) (throwError (StaticError position InvalidLValueArrayLength))
    lValType <- typeCheckExpr lValExpr
    rValType <- typeCheckExpr rValExpr
    unlessM (isSubtypeOf rValType lValType) (throwError (StaticError position (InvalidTypeAssignment rValType lValType))) 

    where
        isLValue :: ExprMeta -> Bool
        isLValue EVar {} = True
        isLValue EArrGet {} = True
        isLValue EMember {} = True
        isLValue _ = False 

typeCheckStmt (Incr position expr) = do
    exprType <- typeCheckExpr expr
    unless (isIntType exprType) (throwError (StaticError position (InvalidTypeIncrementation exprType)))
    whenM (isArrayMemberAccess expr) (throwError (StaticError position InvalidTypeIncrementationArrayLength))

typeCheckStmt (Decr position expr) = do
    exprType <- typeCheckExpr expr
    unless (isIntType exprType) (throwError (StaticError position (InvalidTypeDecrementation exprType)))
    whenM (isArrayMemberAccess expr) (throwError (StaticError position InvalidTypeDecrementationArrayLength))

typeCheckStmt (VRet position) = do
    validReturnType' <- gets validReturnType
    unless (isVoidType validReturnType') (throwError (StaticError position (InvalidReturnType (TVoid Nothing) validReturnType')))

typeCheckStmt (Ret position expr) = do
    exprType <- typeCheckExpr expr
    when (isVoidType exprType) (throwError (StaticError position InvalidReturnTypeVoid))
    validReturnType' <- gets validReturnType
    unlessM (isSubtypeOf exprType validReturnType') (throwError (StaticError position (InvalidReturnType exprType validReturnType')))

typeCheckStmt (Cond position exprCond block) = typeCheckStmt (CondElse position exprCond block (BStmt Nothing (Block Nothing [])))
typeCheckStmt (CondElse position exprCond thenBlock elseBlock) = do
    exprType <- typeCheckExpr exprCond
    unless (isBoolType exprType) (throwError (StaticError position (InvalidTypeCondition exprType)))
    typeCheckBlock (Block Nothing [thenBlock])
    typeCheckBlock (Block Nothing [elseBlock])

typeCheckStmt (While position exprCond block) = do
    exprType <- typeCheckExpr exprCond
    unless (isBoolType exprType) (throwError (StaticError position (InvalidTypeCondition exprType))) 
    typeCheckBlock (Block Nothing [block])

typeCheckStmt (For position type_ ident exprArray (BStmt _ loopBody)) = do
    exprArrayType <- typeCheckExpr exprArray
    unless (isArrayType exprArrayType) (throwError (StaticError position (InvalidTypeForLoopContainer exprArrayType)))
    arrayElementType <- getArrayElementType exprArrayType
    unlessM (isArrayElementType exprArrayType type_) (throwError (StaticError position (InvalidTypeForLoopVariable type_ arrayElementType)))
    variables' <- gets variables
    let stateModifyAction state = state {
        variables = Map.insert ident type_ variables',
        blockDefinedVariables = Set.fromList [ident]
    }
    runLocally stateModifyAction (typeCheckBlockWithCurrentEnv loopBody)
typeCheckStmt (For position type_ ident exprArray loopBody) = typeCheckStmt (For position type_ ident exprArray (BStmt Nothing (Block Nothing [loopBody])))

---------------------- Expressions -------------------------
typeCheckExpr :: ExprMeta -> TypeCheckerMonad TypeMeta
typeCheckExpr (ELitTrue position) = return (TBool position)
typeCheckExpr (ELitFalse position) = return (TBool position)
typeCheckExpr (EString position _) = return (TStr position)
typeCheckExpr (ELitInt position val) = do
    when (val < int32MIN || val > int32MAX) (throwError (StaticError position (IntegerLiteralOutOfBounds val)))
    return (TInt position)
typeCheckExpr (EVar position varName) = getVarType varName position 

typeCheckExpr (EArrGet position arrExpr idxExpr) = do
    idxExprType <- typeCheckExpr idxExpr
    unless (isIntType idxExprType) (throwError (StaticError position (IndexTypeNotInteger idxExprType)))
    arrExprType <- typeCheckExpr arrExpr
    getArrayElementType arrExprType

typeCheckExpr (EMember position exprObject fieldName) = do
    objectType <- typeCheckExpr exprObject
    case objectType of
        (TArray _ _) -> if fieldName == arrayLengthAttribute then return typeInt else throwError (StaticError position (UnknownArrayAttribute fieldName))
        classType -> getMemberTypeField classType fieldName position

typeCheckExpr (EMemberCall position exprClass methodName args) = do
    classType <- typeCheckExpr exprClass
    (returnType, methodArgs) <- getMemberTypeMethod classType methodName position
    actualTypes <- mapM typeCheckExpr args
    checkTypesMatch methodArgs actualTypes position 
    return returnType

typeCheckExpr (EApp position funcName args) = do
    (returnType, funcArgs) <- getFunctionType funcName position
    actualTypes <- mapM typeCheckExpr args
    checkTypesMatch funcArgs actualTypes position
    return returnType

typeCheckExpr (ECast position className) = do
    unlessM (isClassType (TClass Nothing className)) (throwError (StaticError position (ClassNotDefined className)))
    return (TClass position className)

typeCheckExpr (ENewObject position type_) = do
    unlessM (isClassType type_) (throwError (StaticError position (InvalidType type_))) 
    return type_

typeCheckExpr (ENewArray position type_ sizeExpr) = do
    sizeExprType <- typeCheckExpr sizeExpr
    unless (isIntType sizeExprType) (throwError (StaticError position (SizeTypeNotInteger sizeExprType)))
    when (isVoidType type_) (throwError (StaticError position (InvalidType type_)))
    return (TArray position type_)

typeCheckExpr (Neg position expr) = do
    exprType <- typeCheckExpr expr
    unless (isIntType exprType) (throwError (StaticError position (InvalidTypeArithmeticNegation exprType)))
    return (TInt position)

typeCheckExpr (Not position expr) = do
    exprType <- typeCheckExpr expr
    unless (isBoolType exprType) (throwError (StaticError position (InvalidTypeBooleanNegation exprType)))
    return (TBool position)

typeCheckExpr (EMul _ expr1 mulOp expr2) = do
    (exprType1, exprType2) <- typeCheckExpr2 expr1 expr2
    let position = extractMulOpPosition mulOp
    unless (isIntType exprType1 && isIntType exprType2) (case mulOp of
            (Div _) -> throwError (StaticError position (InvalidTypeDivision exprType1 exprType2))
            (Times _) -> throwError (StaticError position (InvalidTypeMultiplication exprType1 exprType2))
            (Mod _) -> throwError (StaticError position (InvalidTypeModulo exprType1 exprType2))   
        )
    return (TInt position)

typeCheckExpr (EAdd position expr1 addOp expr2) = do
    (exprType1, exprType2) <- typeCheckExpr2 expr1 expr2
    let position = extractAddOpPosition addOp
    case addOp of
        (Plus _) -> case (exprType1, exprType2) of
            (TInt _, TInt _) -> return (TInt position)
            (TStr _, TStr _) -> return (TStr position)
            _ -> throwError (StaticError position (InvalidTypeAddition exprType1 exprType2))
        (Minus _) -> do
            unless (isIntType exprType1 && isIntType exprType2) (throwError (StaticError position (InvalidTypeSubtraction exprType1 exprType2)))
            return (TInt position)
    
typeCheckExpr (ERel _ expr1 relOp expr2) = do
    (exprType1, exprType2) <- typeCheckExpr2 expr1 expr2
    let position = extractRelOpPosition relOp
    case (relOp, exprType1, exprType2) of
        (_, TInt _, TInt _) -> return (TBool position)
        (NE _, _, _) | exprType1 == exprType2 -> return (TBool position)
        (EQU _, _, _) | exprType1 == exprType2 -> return (TBool position)
        _ -> throwError (StaticError position (InvalidTypeComparison exprType1 exprType2))

typeCheckExpr (EAnd position expr1 expr2) = do
    (exprType1, exprType2) <- typeCheckExpr2 expr1 expr2
    unless (isBoolType exprType1 && isBoolType exprType2) (throwError (StaticError position (InvalidTypeBooleanAnd exprType1 exprType2)))
    return (TBool position)

typeCheckExpr (EOr position expr1 expr2) = do
    (exprType1, exprType2) <- typeCheckExpr2 expr1 expr2
    unless (isBoolType exprType1 && isBoolType exprType2) (throwError (StaticError position (InvalidTypeBooleanOr exprType1 exprType2)))
    return (TBool position)

typeCheckExpr2 :: ExprMeta -> ExprMeta -> TypeCheckerMonad (TypeMeta, TypeMeta)
typeCheckExpr2 expr1 expr2 = do
    exprType1 <- typeCheckExpr expr1
    exprType2 <- typeCheckExpr expr2
    return (exprType1, exprType2)

-- Checks if given expr evaluates to "arr.length", so that `arr.length = 5;` can be forbidden
isArrayMemberAccess :: ExprMeta -> TypeCheckerMonad Bool
isArrayMemberAccess (EMember _ exprObject fieldName) = do
    objectType <- typeCheckExpr exprObject
    case objectType of
        (TArray _ _) -> return True 
        _ -> return False
isArrayMemberAccess _ = return False

checkValidArrayType :: TypeMeta -> TypeCheckerMonad ()
checkValidArrayType type_@(TArray position _) = do
    unlessM (isTypeDefined type_) (throwError (StaticError position (TypeNotDeclared type_)))
    checkForVoidArray type_
checkValidArrayType type_ = throwError (StaticError (extractTypePosition type_) (InvalidType type_))

checkValidMainFunc :: TypeCheckerMonad ()
checkValidMainFunc = do
    mainFunctionType <- getFunctionType mainFunctionIdent Nothing
    unless (isValidMainFunctionType mainFunctionType) (throwError (StaticError Nothing (InvalidMainFunctionType mainFunctionType validMainFunctionType)))

------------------------ Run ------------------------
runTypeChecker :: ProgramMeta -> Either StaticError FrontendResult 
runTypeChecker program@(Program _ topDefs)= do
    let result = runExcept $ runStateT (addBuiltInFunctions >> runTypeChecker') newTypeCheckerState in
        case result of
            Left error -> Left error
            Right (program, env) -> Right (program, functions env, classes env)
    where
        runTypeChecker' = do
            classNameToMetadata <- extractClasses topDefs
            modify (\state -> state { classes = classNameToMetadata })
            forM_ topDefs registerFunction
            -- TODO simplify or check first... ?
            -- let programSimplified@(Program _ topDefs') = simplifyProgram program
            checkValidMainFunc
            forM_ topDefs typeCheckTopDef 
            -- return (programSimplified)
            let programSimplified = simplifyProgram program
            dropUnreachableCode programSimplified 


---------------------- Unreachable Code ------------------------
dropUnreachableCode :: ProgramMeta -> TypeCheckerMonad ProgramMeta 
dropUnreachableCode (Program position topDefs) = do
    topDefs' <- mapM processTopDef topDefs
    return (Program position topDefs')

    where
        processTopDef (TopClassDef position classDef) = do
            classDefProcessed <- processClassDef classDef
            return (TopClassDef position classDefProcessed)
        processTopDef (TopFuncDef position funcDef) = do
            funcDefProcessed <- processFuncDef funcDef
            return (TopFuncDef position funcDefProcessed)
    
        processClassDef (ClassDef position className classMembers) = do
            processedClassMembers <- mapM processClassMember classMembers
            return (ClassDef position className processedClassMembers)
        processClassDef (ClassExtDef position className parentClass classMembers) = do
            processedClassMembers <- mapM processClassMember classMembers
            return (ClassExtDef position className parentClass processedClassMembers) 
        
        processClassMember original@ClassField {} = return original
        processClassMember (ClassMethod position funcDef) = do
            processedFuncDef <- processFuncDef funcDef
            return (ClassMethod position processedFuncDef)
        
        processFuncDef funcDef@(FuncDef position retType name args body) = do
            bodyOptimized <- processFuncBody funcDef
            return (FuncDef position retType name args bodyOptimized)

        processFuncBody :: FuncDefMeta -> TypeCheckerMonad BlockMeta
        processFuncBody (FuncDef position retType name _ (Block _ stmts)) = do
            let (optimizedBlock, hasMissingReturn) = runState (optimizeBlock stmts) True
            when (hasMissingReturn && not (isVoidType retType)) (throwError (StaticError position (MissingReturn name)))
            return (Block Nothing (dropWhileEnd (== VRet Nothing) optimizedBlock))

        optimizeBlock :: [StmtMeta] -> State Bool [StmtMeta]
        optimizeBlock stmts = do
            stmtsOptimized <- mapM optimize stmts
            return (concat stmtsOptimized)
            
            where 
                optimize stmt = do
                    reachable <- get
                    if reachable 
                        then optimizeStmt stmt
                        else return []

        optimizeStmt :: StmtMeta -> State Bool [StmtMeta]
        optimizeStmt (BStmt _ (Block _ [])) = return [] 
        optimizeStmt (BStmt _ (Block _ [BStmt _ block])) = optimizeStmt (BStmt Nothing block)
        optimizeStmt (BStmt _ (Block _ stmts)) = do
            stmts' <- optimizeBlock stmts
            return [BStmt Nothing (Block Nothing stmts')]
        optimizeStmt (Cond _ (ELitFalse _) _) = return []
        optimizeStmt (Cond _ (ELitTrue _) stmt) = optimizeStmt (packStmtToBStmt stmt) 
        optimizeStmt (CondElse _ (ELitTrue _) thenStmt _) = optimizeStmt (packStmtToBStmt thenStmt)
        optimizeStmt (CondElse _ (ELitFalse _) _ elseStmt) = optimizeStmt (packStmtToBStmt elseStmt)
        optimizeStmt o@(CondElse _ condExpr thenStmt elseStmt) = do
            wasReachable <- get -- was this code reachable before "if"
            thenStmtOptimized <- optimizeBlock [thenStmt]
            reachableAfterThen <- get -- is code AFTER if-else block reachable if thenStmt was executed?
            put wasReachable -- fresh start
            elseStmtOptimized <- optimizeBlock [elseStmt]
            reachableAfterElse <- get -- is code AFTER if-else block reachable if elseStmt was executed?
            if reachableAfterThen || reachableAfterElse 
                then put wasReachable -- if at some case after if-else block was reachable then nothing has changed 
                else put False  -- otherwise both thenStmt and elseStmt must have return
            return [CondElse Nothing condExpr (packStmtToBStmtN thenStmtOptimized) (packStmtToBStmtN elseStmtOptimized)]

        optimizeStmt (For _ type_ ident expr loopBlock) = do
            loopBlockOptimized <- optimizeBlock [loopBlock]
            -- simply preserve 'reachability' is state (set when checking loopBlock)
            return [For Nothing type_ ident expr (packStmtToBStmtN loopBlockOptimized)]

        optimizeStmt (While _ (ELitFalse _) _) = return []
        optimizeStmt (While _ (ELitTrue _) loopBlock) = do
            loopBlockOptimized <- optimizeBlock [loopBlock] -- "pack back" optimized body. It has to be a stmt, so make it a BStmt
            put False -- while(true){} so no further code is reachable (we have no 'break')
            return [While Nothing (ELitTrue Nothing) (packStmtToBStmtN loopBlockOptimized)]
        
        optimizeStmt res@(VRet _) = do
            put False
            return [res]
        optimizeStmt res@(Ret _ _) = do
            put False
            return [res]
        optimizeStmt stmt = return [stmt]
       
        packStmtToBStmt :: StmtMeta -> StmtMeta
        packStmtToBStmt stmt = BStmt Nothing (Block Nothing [stmt])
        packStmtToBStmtN :: [StmtMeta] -> StmtMeta
        packStmtToBStmtN stmts = BStmt Nothing (Block Nothing stmts)