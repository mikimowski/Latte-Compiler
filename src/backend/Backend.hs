module Backend where

import Compiler

-- import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Stack
import Common
import Assemblyx86
import AbsLatte
import CompilerUtils
import Utils
import DList
import qualified OrderedMap as OM
import qualified CodeOptimizer as CO


flagOptimize = "opt"
flagVerbose = "verbose"
backendFlags = [flagOptimize, flagVerbose]
type BackendFlags = [String]

backend :: BackendFlags -> FrontendResult -> Assemblyx86Program
backend flags (program@(Program _ topDefs), functionsTypes, classesTypes) = let finalState = runCompiler program functionsTypes classesTypes in
    [joinLines $ generateClassesVTablesDefs classesWithMethods
        ++ [""] ++ generateDataSection (constStrToLabel finalState)
        ++ [""] ++ generateTextSection funcNames (CO.optimizeCode $ DList.toList $ generatedCode finalState)]

    where
        funcNames = map (\(TopFuncDef _ (FuncDef _ _ funcName _ _)) -> funcName) (filterTopFuncDef topDefs)
        classesWithMethods = mapMaybe (\(className, classMetadata) -> 
            let methods' = OM.toList (methods classMetadata) in 
                if not (null methods') then Just (className, methods') else Nothing) (Map.toList classesTypes)

        generateClassesVTablesDefs :: [(ClassName, [(MemberName, (Int, FuncType, MemberSource))])] -> [Assemblyx86Code]
        generateClassesVTablesDefs = map (\(className, methods) ->
            let classLabel = getVTableLabel className in
                let methodsLabels =  map (\(methodName, (_, _, sourceClass)) -> getMethodLabel sourceClass methodName) methods
                    in classLabel ++ ":\n    .int " ++ intercalate "\n    .int " methodsLabels)

        generateDataSection :: Map.Map String  Label -> [Assemblyx86Code]
        generateDataSection constStringLabels = ".data\n" : generateConstStringsDefs constStringLabels
            where
                generateConstStringsDefs :: Map.Map String Label -> [Assemblyx86Code]
                generateConstStringsDefs = let constStringDefRefCnt = 42 in 
                    map (\(str, label) -> label ++ ":\n    .int " ++ show constStringDefRefCnt ++ "\n    .asciz " ++ showConstStr str) . Map.toAscList 

        generateTextSection :: [Ident] -> [AssOp] -> [Assemblyx86Code]
        generateTextSection funcNames generatedCode = ".text\n" :
            generateGlobalFuncDefs funcNames ++ [""] ++ convertToProgram generatedCode 
            where
                generateGlobalFuncDefs :: [Ident] -> [Assemblyx86Code]
                generateGlobalFuncDefs = map (\funcName -> ".global " ++ show funcName)

        showConstStr :: String -> String
        showConstStr "" = "\"\""
        showConstStr str = (show . purifyString) str
        
        joinLines = intercalate "\n" . dropWhile null