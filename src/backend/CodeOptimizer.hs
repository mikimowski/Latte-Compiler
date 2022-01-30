
{-# LANGUAGE LambdaCase #-}

module CodeOptimizer where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Assemblyx86
import CompilerUtils
import Common


optimizeCode :: [AssOp] -> [AssOp]
optimizeCode code = let nIterations = 13 in
    -- uncomment this for info labels && rather unoptmized code
    -- iterate (removeDoubleNewline . removeTrivialPopPush . removeTrivialPushPop . removeTrivialMoves) (removeDeadCode $ code) !! nIterations
    iterate (removeTrivialCode . removeTrivialJumps . removeDoubleNewline . inlineTrivialFunctions . simplifyTrivialFunctions . removeTrivialMoves . removeTrivialPopPush . removeTrivialPushPop) (removeInfoLabels . removeDeadCode $ code) !! nIterations

removeTrivialJumps :: [AssOp] -> [AssOp]
removeTrivialJumps = foldr (\prevOp code -> 
    case code of
        [] -> [prevOp]
        (nextOp:_) -> case unpackJump prevOp of
            Just lbl -> if isTrivialJump lbl nextOp
                then code
                else prevOp:code
            Nothing -> prevOp:code 
    ) [] 
    
    where
        isTrivialJump lbl (LABEL lbl') = lbl == lbl' 
        isTrivialJump _ _ = False

removeTrivialPushPop :: [AssOp] -> [AssOp]
removeTrivialPushPop = foldr (\prevOp code -> 
    case code of
        [] -> [prevOp]
        (nextOp:codeSimpler) -> case getPushedReg prevOp of
            Just reg -> if isTrivialPushPop reg nextOp
                then codeSimpler -- without (Push, Pop)
                else prevOp:code
            Nothing -> prevOp:code 
    ) [] 
    
    where
        getPushedReg op = case op of
            PUSH (Reg reg) -> Just reg
            _ -> Nothing
        
        isTrivialPushPop reg (POP (Reg reg')) = reg == reg' 
        isTrivialPushPop _ _ = False       

removeTrivialCode :: [AssOp] -> [AssOp]
removeTrivialCode = foldr (\prevOp code -> 
    let originalCode = prevOp:code in
        case code of
            [] -> [prevOp]
            -- pop reg2
            -- addl reg1 reg2
            -- push reg2
            ADDL (Reg reg1) (Reg reg2):PUSH (Reg reg3):ops -> case prevOp of
                POP (Reg reg0) -> if reg0 == reg2 && reg2 == reg3 then ADDL (Reg reg1) (MemoryLoc ESP):ops else originalCode
                _ -> originalCode
            SUBL (Reg reg1) (Reg reg2):PUSH (Reg reg3):ops -> case prevOp of
                POP (Reg reg0) -> if reg0 == reg2 && reg2 == reg3 then SUBL (Reg reg1) (MemoryLoc ESP):ops else originalCode
                _ -> originalCode
            _ -> originalCode
        ) [] 

-- substitutes
-- POP %EAX
-- PUSH %EAX
-- with
-- MOVL %(ESP) %EAX
removeTrivialPopPush :: [AssOp] -> [AssOp]
removeTrivialPopPush = foldr (\prevOp code -> 
    let originalCode = prevOp:code in
        case code of
            [] -> [prevOp]
            (nextOp:codeSimpler) -> case getPopedReg prevOp of
                Just reg -> if isTrivialPopPush reg nextOp
                    then makeMoveOp reg:codeSimpler -- without (Push, Pop)
                    else originalCode
                Nothing -> originalCode 
        ) [] 
    
    where
        getPopedReg op = case op of
            POP (Reg reg) -> Just reg
            _ -> Nothing
        
        isTrivialPopPush reg (PUSH (Reg reg')) = reg == reg' 
        isTrivialPopPush _ _ = False

        makeMoveOp reg = MOVL (MemoryLoc ESP) (Reg reg)      

-- e.g. MOVL (IntLit v) (Reg EAX)
-- e.g. MOVL (Reg EAX) (Reg EAX)
removeTrivialMoves :: [AssOp] -> [AssOp]
removeTrivialMoves = removeTrivialMoves' . foldr(\prevOp code ->
    let originalCode = prevOp:code in
    case code of
        [] -> [prevOp]
        nextOp:ops -> case (prevOp, nextOp) of
            (MOVL (IntLit v1) (Reg reg1), MOVL (IntLit v2) (Reg reg2)) -> if v1 == v2 && reg1 == reg2
                then code
                else originalCode
            _ -> originalCode 
        ) []

    where
        removeTrivialMoves' = filter (\case
            MOVL (Reg reg) (Reg reg') -> reg /= reg' 
            _ -> True)

removeInfoLabels :: [AssOp] -> [AssOp]
removeInfoLabels = foldr (\prevOp code ->
    case prevOp of
        LABEL label -> if isInfoLabel label then code else prevOp:code
        _ -> prevOp:code) []

removeDeadCode :: [AssOp] -> [AssOp]
removeDeadCode = fst . removeJumpsToRemovedLabels . foldr (\prevOp (code, removedLabels) -> 
    case code of
        [] -> ([prevOp], removedLabels)
        (nextOp:ops) -> case nextOp of
            LABEL label -> if labelInfoFnEnd `isInfixOf` label 
                then (case prevOp of         -- remove prev dead operation 
                    RET -> (prevOp:code, removedLabels)
                    LABEL label -> if whileEndLabel `isInfixOf` label 
                        then (prevOp:code, removedLabels)
                        else (code, Set.insert label removedLabels)  -- if label then memorize its removal
                    NEWLINE -> (nextOp:prevOp:ops, removedLabels)                  -- swap to keep newline
                    _ -> (code, removedLabels))      -- just remove operation
                else (                                  
                    prevOp:code, removedLabels
                    )
            _ -> (prevOp:code, removedLabels)        
                ) ([], Set.empty)
    
    where
        removeJumpsToRemovedLabels (ops, removedLabels') = foldr (\prevOp (code, removedLabels) ->
            case code of
                [] -> ([prevOp], removedLabels)
                (nextOp:_) -> case unpackJump prevOp of
                    Just lbl -> if Set.member lbl removedLabels 
                        then (code, removedLabels)
                        else (prevOp:code, removedLabels)
                    Nothing -> (prevOp:code, removedLabels)
            ) ([], removedLabels') ops 

unpackJump :: AssOp -> Maybe Label
unpackJump op = case op of
    JMP lbl -> Just lbl
    JZ lbl -> Just lbl
    JNZ lbl -> Just lbl
    JE lbl -> Just lbl
    JNE lbl -> Just lbl
    JGE lbl -> Just lbl
    JLE lbl -> Just lbl
    JL lbl -> Just lbl
    _ -> Nothing



-- Trivial functions:
    -- push %ebp
    -- movl %esp, %ebp
    -- leave
    -- ret

    -- push %ebp
    -- movl %esp, %ebp
    -- movl $0, %eax
    -- leave
    -- ret
simplifyTrivialFunctions :: [AssOp] -> [AssOp]
simplifyTrivialFunctions = foldr (\prevOp code -> 
    case code of
        [] -> [prevOp]
        [uno] -> prevOp:code
        [dos, uno] -> prevOp:code
        [tres, dos, uno] -> prevOp:code
        [cuatro, tres, dos, uno] -> prevOp:code
        PUSH (Reg EBP):MOVL (Reg ESP) (Reg EBP):LEAVE:RET:ops -> prevOp:RET:ops
        PUSH (Reg EBP):MOVL (Reg ESP) (Reg EBP):MOVL (IntLit v) (Reg EAX):LEAVE:RET:ops -> prevOp:MOVL (IntLit v) (Reg EAX):RET:ops
        _ -> prevOp:code
    ) [] 

inlineTrivialFunctions :: [AssOp] -> [AssOp]
inlineTrivialFunctions = inlineCallsToTrivialFunctions . extractTrivialFunctionsPreserve
    
    where
        inlineCallsToTrivialFunctions :: ([AssOp], Map.Map String [AssOp]) -> [AssOp]
        inlineCallsToTrivialFunctions (ops, trivialFn') = fst $ foldr(\prevOp (code, trivialFn) ->
            let noChange = prevOp:code in
            case prevOp of
                CALL label -> case Map.lookup label trivialFn of
                    Just body -> (body ++ code, trivialFn) 
                    Nothing -> (noChange, trivialFn)
                _ -> (noChange, trivialFn)
            ) ([], trivialFn') ops 

        extractTrivialFunctionsRemove :: [AssOp] -> ([AssOp], Map.Map String [AssOp])
        extractTrivialFunctionsRemove = foldr(\prevOp (code, trivialFn) ->
            let originalCode = prevOp:code in
            case code of
                [] -> ([prevOp], trivialFn)
                [uno] -> case (prevOp, uno) of
                    -- p:
                    --   ret
                    -- into []
                    (FNLABEL fnName, op) -> skipMain originalCode [] fnName [] trivialFn
                    _ -> (prevOp:code, trivialFn)
                dos:uno:ops -> case (prevOp, dos, uno) of
                    -- p:
                    --   ret
                    -- into: []
                    (FNLABEL fnName, RET, _) -> let inlinedFnBody = [] in
                        skipMain originalCode (uno:ops) fnName inlinedFnBody trivialFn
                    -- p:
                    --   movl $v, $eax
                    --   ret
                    -- into: movl $v, %eax
                    (FNLABEL fnName, MOVL src dst, RET) -> let inlinedFnBody = [MOVL src dst] in 
                        skipMain originalCode ops fnName inlinedFnBody trivialFn
                    _ -> (prevOp:code, trivialFn)
            ) ([], Map.empty)

        extractTrivialFunctionsPreserve :: [AssOp] -> ([AssOp], Map.Map String [AssOp])
        extractTrivialFunctionsPreserve = foldr(\prevOp (code, trivialFn) ->
            let originalCode = prevOp:code in
            case code of
                [] -> ([prevOp], trivialFn)
                [uno] -> case (prevOp, uno) of
                    (FNLABEL fnName, op) -> skipMain originalCode originalCode fnName [] trivialFn
                    _ -> (originalCode, trivialFn)
                dos:uno:ops -> case (prevOp, dos, uno) of
                    (FNLABEL fnName, RET, _) -> let inlinedFnBody = [] in
                        skipMain originalCode originalCode fnName inlinedFnBody trivialFn
                    (FNLABEL fnName, MOVL src dst, RET) -> let inlinedFnBody = [MOVL src dst] in 
                        skipMain originalCode originalCode fnName inlinedFnBody trivialFn
                    _ -> (originalCode, trivialFn)
            ) ([], Map.empty)


        skipMain originalCode newCode fnName inlinedFnBody trivialFn = if fnName /= mainFunctionLabel
            then (newCode, Map.insert fnName inlinedFnBody trivialFn)
            else (originalCode, trivialFn)

-- estetic helper
removeDoubleNewline :: [AssOp] -> [AssOp]
removeDoubleNewline = foldr (\prevOp code ->
    case code of
        [] -> [prevOp]
        [NEWLINE] -> [prevOp]
        NEWLINE:ops -> case prevOp of
            NEWLINE -> code
            _ -> prevOp:code
        _ -> prevOp:code
    ) []