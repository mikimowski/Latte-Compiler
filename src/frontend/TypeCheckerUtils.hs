module TypeCheckerUtils where

-- Library
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph
-- Custom
import Data.Maybe
import Data.List
import OrderedMap

import StaticError
import AbsLatte
import Common
import Utils


-------- Monad & State --------
data TypeCheckerState = TypeCheckerState {
    functions :: FunctionsTypes,
    classes :: ClassesTypes,
    variables :: Map.Map Ident VarType,
    blockDefinedVariables :: Set.Set Ident, -- all we need is set to prevent double declaration within the same block
    validReturnType :: TypeMeta  -- expected return type for a current function
}

newTypeCheckerState = TypeCheckerState {
    functions = Map.empty,
    classes = undefined, 
    variables = Map.empty, 
    blockDefinedVariables = Set.empty,
    validReturnType = undefined
}

type TypeCheckerMonad = StateT TypeCheckerState (Except StaticError)

unlessM :: TypeCheckerMonad Bool -> TypeCheckerMonad () -> TypeCheckerMonad ()
unlessM condM action = do
  cond <- condM
  unless cond action

whenM :: TypeCheckerMonad Bool -> TypeCheckerMonad () -> TypeCheckerMonad ()
whenM condM action = do
  cond <- condM
  when cond action

---------- CLASSES ----------
type ClassFullInfo = (ClassName, (Maybe ClassName, [ClassMemberMeta], Position))
type ClassFullInfo2 = ((ClassName, Maybe ClassName), (ClassMembers (TypeMeta, MemberSource), ClassMembers (FuncType, MemberSource)))

extractClasses :: [TopDefMeta] -> TypeCheckerMonad (Map.Map ClassName ClassMetadata)
extractClasses topDefs = do 
    let topClassDefs = filterTopClassDef topDefs
        classesInfo = mapMaybe extractClassFullInfo topClassDefs -- mapMaybe "filters" Nothing from the given list
        classNameToInfo = Map.fromList classesInfo
        definedClassesNames = extractClassesNames classesInfo
        classWithParent = [(className, parentClassName) | (className, (Just parentClassName, _, _)) <- classesInfo]
    
    checkForClassRedeclaration topClassDefs 
    validateInheritance classesInfo definedClassesNames
    -- [((className, parentClass), (unionFields, unionMethod))]
    classesFullInfo <- mapM (inherit classNameToInfo) (Set.toList definedClassesNames)
    return $ buildClassMetadataMap classesFullInfo

    where        
        filterTopClassDef = filter (\topDef -> case topDef of 
            (TopClassDef _ _) -> True
            _ -> False)

        checkForClassRedeclaration :: [TopDefMeta] -> TypeCheckerMonad ()
        checkForClassRedeclaration topClassDefs = do
            let classesNamesCount = count $ map (\(TopClassDef _ classDef) -> case classDef of
                    (ClassDef _ className _) -> className
                    (ClassExtDef _ className _ _) -> className
                    ) topClassDefs
            forM_ classesNamesCount (\(className, count) -> when (count > 1) $ throwError (StaticError Nothing (ClassRedeclaration className)))
        
        extractClassFullInfo :: TopDefMeta -> Maybe ClassFullInfo
        extractClassFullInfo (TopClassDef _ (ClassDef position className classMembers)) = Just (className, (Nothing, classMembers, position))
        extractClassFullInfo (TopClassDef _ (ClassExtDef position className parentClassName classMembers)) = Just (className, (Just parentClassName, classMembers, position))
        
        extractClassesNames :: [ClassFullInfo] -> Set.Set ClassName
        extractClassesNames = Set.fromList . map (\(className, (_, _, _)) -> className)

        validateInheritance :: [ClassFullInfo] -> Set.Set ClassName -> TypeCheckerMonad ()
        validateInheritance classesInfo definedClassesNames = do               
            let inheritanceEdgesWithPosition = [(parentClassName, className, position) | (className, (Just parentClassName, _, position)) <- classesInfo]
                inheritanceEdges = [(parentClassName, className) | (className, (Just parentClassName, _, _)) <- classesInfo] 
            checkParentExistence inheritanceEdgesWithPosition
            checkForCycleInheritance inheritanceEdges
            
            where
                checkParentExistence :: [(ClassName, ClassName, Position)] -> TypeCheckerMonad ()
                checkParentExistence inheritanceEdges = 
                    forM_ inheritanceEdges (\(parentClassName, className, position) ->
                        unless (Set.member parentClassName definedClassesNames) (throwError (StaticError position (UndefinedClassInheritance className parentClassName))))                           

                checkForCycleInheritance :: [(ClassName, ClassName)] -> TypeCheckerMonad ()
                checkForCycleInheritance edges = case findCycle edges of
                    Just cycle -> throwError (StaticError Nothing (CycleInheritance cycle))
                    Nothing -> return ()

        -- Recursively extracts attributed and methods for the given class
        -- Note: After passing classNameToInfo map, 'memoize' function is instantiated.
        -- Thus it avoids O(n^2) complexity for calculating inheritance
        -- Then mapM is called to perform this action for each class we have.
        -- Full class info is returned
        -- returns: [((className, parentClass), (unionFields, unionMethod))]
        inherit :: Map.Map ClassName (Maybe ClassName, [ClassMemberMeta], Position) -> Ident -> TypeCheckerMonad ClassFullInfo2
        inherit classNameToInfo className = do
            let (parentClass, classMembers, position) = (Map.!) classNameToInfo className
            let classMethodsDefs = filterClassMethods classMembers
            let classFieldsDefs = filterClassFields classMembers 
            let classFieldsUnchecked = map (\(ClassField _ fieldType fieldName) -> (fieldName, fieldType)) classFieldsDefs
            let classMethodsUnchecked = map (\(ClassMethod _ funcDef@(FuncDef _ _ methodName _ _)) -> (methodName, getFuncType funcDef)) classMethodsDefs

            -- Note: fromList will additionaly return in formation about types mismatch, but it WON'T check for duplicated names.
            -- If desired, another check is required for that
            classFields' <- fromEitherM (OrderedMap.fromList classFieldsUnchecked) (\(fieldName, _, _) -> throwError (StaticError position (DuplicateAttribute fieldName className)))
            classMethods' <- fromEitherM (OrderedMap.fromList classMethodsUnchecked) (\(methodName, _, _) -> throwError (StaticError position (DuplicateMethod methodName className)))
            let classFields = OrderedMap.mapVals (\v -> (v, className)) classFields'
            let classMethods = OrderedMap.mapVals (\v -> (v, className)) classMethods'

            case parentClass of
                Nothing -> return ((className, Nothing), (classFields, classMethods))
                Just parentClassName -> do
                    (_, (inheritedFields, inheritedMethods)) <- memoClassMembersInheritance parentClassName 
                
                    let fieldsUnionAllEqual = OrderedMap.unionEq inheritedFields classFields (\(type1, _) (type2, _) -> type1 == type2)
                    fieldsUnion <- fromEitherM fieldsUnionAllEqual (\(fieldName, (type1, sourceClassName), (type2, _)) -> throwError (StaticError Nothing (InheritanceAttributeTypeMismatch fieldName type2 className type1 sourceClassName)))
                    let methodsUnionAllEqual = OrderedMap.unionEq inheritedMethods classMethods (\(type1, _) (type2, _) -> type1 == type2)
                    methodsUnion <- fromEitherM methodsUnionAllEqual (\(methodName, (type1, sourceClassName), (type2, _)) -> throwError (StaticError Nothing (InheritanceMethodTypeMismatch methodName type2 className type1 sourceClassName)))
                    return ((className, parentClass), (fieldsUnion, methodsUnion))

            where 
                memoClassMembersInheritance = memoize $ inherit classNameToInfo

        buildClassInfoMap :: (Eq a, Eq b) => ClassMembers (a, b) -> ClassMembers (Int, a, b)
        buildClassInfoMap m = let (Right m') = OrderedMap.fromList [(key, (idx, a, b)) | ((key, (a, b)), idx) <- zip (OrderedMap.toList m) [0..]] in m'

        buildClassMetadataMap :: [ClassFullInfo2] -> Map.Map ClassName ClassMetadata
        buildClassMetadataMap = Map.fromList . map (\((className, parentClass), (classFields, classMethods)) ->
            (className, ClassMetadata {
                name = className,
                parentClass = parentClass,
                fields = buildClassInfoMap classFields,
                methods = buildClassInfoMap classMethods
            }))

-- https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Graph.html
findCycle :: Ord a => [(a, a)] -> Maybe [a]
findCycle edges =
    let adjacencyList = buildAdjacencyList edges
        graph = map (\(node, neighbors) -> (node, node, neighbors)) adjacencyList

    -- graph: a list of nodes uniquely identified by keys, with a list of keys of nodes this node has edges to
    -- graph := [(node, key, [key])]. In code it will be called in form [(className, className, [childClassName])]
    in findCycle' (Graph.stronglyConnComp graph)

    where
        -- Transforms list of edges into adjacencyList. Namely, list of pair (node, [neighbors])
        buildAdjacencyList :: Ord a => [(a, a)] -> [(a, [a])] 
        buildAdjacencyList edges =
            let uniqueVertices = Set.toList $ Set.fromList $ concatMap (\(u, v) -> [u, v]) edges 
                accumulatorMap = Map.fromList $ map (\v -> (v, [])) uniqueVertices -- Mapping: node -> []
            in Map.toList $ Data.List.foldl' (\accMap (k, v) -> Map.adjust (v:) k accMap) accumulatorMap edges
            
        findCycle' :: [Graph.SCC a] -> Maybe [a]
        findCycle' [] = Nothing
        findCycle' (scc:sccs) = case scc of
            Graph.AcyclicSCC _ -> findCycle' sccs
            Graph.CyclicSCC cycle@(node:_) -> Just (cycle ++ [node])


-------------------- State Modification --------------------
declareVar :: Ident -> TypeMeta -> TypeCheckerMonad ()             
declareVar varName varType = do
    blockDefinedVariables' <- gets blockDefinedVariables
    let isAlreadyDefined = Set.member varName blockDefinedVariables'
    when isAlreadyDefined (throwError (StaticError (extractTypePosition varType) (VariableRedeclaration varName)))
    variables' <- gets variables
    modify (\state -> state {
        variables = Map.insert varName varType variables',
        blockDefinedVariables = Set.insert varName blockDefinedVariables'
    })

----- Builts in functions -----
addBuiltInFunctions :: TypeCheckerMonad ()
addBuiltInFunctions = 
    mapM_ (\(funcName, returnType, argsTypes) -> do
        addFuncType funcName returnType argsTypes
    ) builtInFunctions


-- Adds function type to the map
addFuncType :: Ident -> TypeMeta -> [TypeMeta] -> TypeCheckerMonad ()
addFuncType funcName returnType argTypes = modify (\state -> state {
    functions = Map.insert funcName (returnType, argTypes) (functions state)
})


-------------------- Get or throwError --------------------
getVarType :: Ident -> Position -> TypeCheckerMonad TypeMeta
getVarType varName errorPosition = do
    variables' <- gets variables
    case Map.lookup varName variables' of
        Just type_ -> return type_
        Nothing -> throwError (StaticError errorPosition (VariableNotDefined varName))

getFunctionType :: Ident -> Position -> TypeCheckerMonad FuncType
getFunctionType funcName errorPosition = do
    functions' <- gets functions
    case Map.lookup funcName functions' of
        Just type_ -> return type_
        Nothing -> throwError (StaticError errorPosition (FunctionNotDefined funcName))

getMemberTypeMethod :: TypeMeta -> Ident -> Position -> TypeCheckerMonad FuncType 
getMemberTypeMethod classType@(TClass _ className) methodName errorPosition = do
    unlessM (isClassType classType) (throwError (StaticError errorPosition (InvalidTypeMemberAccessNotAClass classType)))
    classes' <- gets classes
    let classMetadata = (Map.!) classes' className -- checked above that's it's an existing class
    case (OrderedMap.!?) (methods classMetadata) methodName of
        Nothing -> throwError (StaticError errorPosition (ClassMemberNotExist className methodName))
        Just (_, type_, _) -> return type_
getMemberTypeMethod type_ _ errorPosition = throwError (StaticError errorPosition (InvalidTypeMemberAccessNotAClass type_))

getMemberTypeField :: TypeMeta -> Ident -> Position -> TypeCheckerMonad VarType 
getMemberTypeField classType@(TClass _ className) fieldName errorPosition = do
    unlessM (isClassType classType) (throwError (StaticError errorPosition (InvalidTypeMemberAccessNotAClass classType)))
    classes' <- gets classes
    let classMetadata = (Map.!) classes' className -- checked above that's it's an existing class
    case (OrderedMap.!?) (fields classMetadata) fieldName of
        Nothing -> throwError (StaticError errorPosition (ClassMemberNotExist className fieldName))
        Just (_, type_, _) -> return type_
getMemberTypeField type_ _ errorPosition = throwError (StaticError errorPosition (InvalidTypeMemberAccessNotAClass type_))

getClassMetadata :: ClassName -> Position -> TypeCheckerMonad ClassMetadata
getClassMetadata className errorPosition = do
    classes' <- gets classes
    case Map.lookup className classes' of
        Just classMetadata -> return classMetadata
        Nothing -> throwError (StaticError errorPosition (ClassNotDefined className))

checkTypesMatch :: [TypeMeta] -> [TypeMeta] -> Position -> TypeCheckerMonad () 
checkTypesMatch expectedTypes actualTypes errorPosition = do
    let (len1, len2) = (length actualTypes, length expectedTypes)
    unless (len1 == len2) (throwError (StaticError errorPosition (InvalidNumberOfArguments len1 len2)))
    forM_ (zip3 [1..] expectedTypes actualTypes) (\(idx, expectedType, actualType) -> 
        unlessM (isSubtypeOf actualType expectedType) (throwError (StaticError errorPosition (ArgumentTypeMismatch idx actualType expectedType)))  
        )

checkForVoidArray :: TypeMeta -> TypeCheckerMonad ()
checkForVoidArray type_@(TArray _ t) = case t of
    (TVoid position) -> throwError (StaticError position (InvalidType type_))
    _ -> return ()
checkForVoidArray _ = return () 

getArrayElementType :: TypeMeta -> TypeCheckerMonad TypeMeta 
getArrayElementType (TArray _ t) = return t
getArrayElementType t = throwError (StaticError (extractTypePosition t) (InvalidTypeArrayGet t))


-------------------- Boolean Check --------------------
-- Checks if type1 is subtype of type2. For 'simple types' is == True,
-- but for custom types (classes) we need to check inheritance path
-- Note: type is valid subtype of itself
isSubtypeOf :: TypeMeta -> TypeMeta -> TypeCheckerMonad Bool
isSubtypeOf (TClass _ type1) (TClass _ type2) = do
    classes' <- gets classes
    return $ elem type2 (type1 : getAncestorsOf type1 classes')
    where 
        getAncestorsOf :: ClassName -> Map.Map ClassName ClassMetadata -> [ClassName]
        getAncestorsOf className classNameToInfo =
            let classInfo = (Map.!) classNameToInfo className in
                case parentClass classInfo of
                    Just parentClassName -> parentClassName : getAncestorsOf parentClassName classNameToInfo
                    Nothing -> []
isSubtypeOf type1 type2 = return (type1 == type2)

isTypeDefined :: TypeMeta -> TypeCheckerMonad Bool
isTypeDefined (TClass _ className) = Map.member className <$$> classes
isTypeDefined (TArray _ type_) = isTypeDefined type_
isTypeDefined _ = return True

isClassType :: TypeMeta -> TypeCheckerMonad Bool
isClassType (TClass _ className) = Map.member className <$$> classes 
isClassType _ = return False

isArrayElementType :: TypeMeta -> TypeMeta -> TypeCheckerMonad Bool
isArrayElementType (TArray _ elementType) type_ = isSubtypeOf elementType type_
isArrayElementType _ _ = return False

-------------------- Extractors --------------------
extractTypePosition :: TypeMeta -> Position
extractTypePosition t = case t of
    (TClass position _) -> position
    (TBool position) -> position
    (TStr position) -> position
    (TInt position) -> position
    (TVoid position) -> position
    (TArray position _) -> position

extractRelOpPosition :: RelOpMeta -> Position
extractRelOpPosition relOp = case relOp of
    (LTH position) -> position
    (LE position) -> position
    (GTH position) -> position
    (GE position) -> position
    (EQU position) -> position
    (NE position) -> position

extractAddOpPosition :: AddOpMeta -> Position
extractAddOpPosition mulOp = case mulOp of
    (Plus position) -> position
    (Minus position) -> position

extractMulOpPosition :: MulOpMeta -> Position
extractMulOpPosition mulOp = case mulOp of
    (Div position) -> position
    (Times position) -> position
    (Mod position) -> position
