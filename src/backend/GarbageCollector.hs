
module GarbageCollector where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import AbsLatte
import Assemblyx86
import CompilerUtils
import Common


emitGarbageCollector = do
    emitDefaultDestructor 
    emitArrayComplexDestructor

initRefCnt reg = do
    emitLabel =<< getNewLabelCustom labelInfoRefCntInit
    emitAssOp $ MOVL (IntLit 1) (MemoryLocOffset reg refCounterOffset)

-- Expects register holding address of the OBJECT
-- It can be Null
-- Operation preserves address in the given register 
incrRefCnt reg = emitAssOps [
    PUSH reg,
    CALL libIncrRefCounter,
    POP reg
    ]

incrRefCntSafe :: TypeMeta -> Operand -> CompilerMonad () 
incrRefCntSafe type_ varMemoryLoc = when (isComplexType type_) $
    emitAssOps [
        PUSH varMemoryLoc, --(Reg EAX),
        CALL libIncrRefCounter,
        ADDL (IntLit dWord) (Reg ESP)
        ]


-- safe version, performs check for null and complex type 
decrRefCntSafe :: TypeMeta -> Operand -> CompilerMonad () 
decrRefCntSafe type_ refMemoryLoc = when (isComplexType type_) (do
    labelBegin <- getNewLabelCustom labelInfoDecrRefCntSafeEntry
    labelAfter <- getNewLabelCustom "decr_ref_cnt_safe_end"
    emitLabel labelBegin
    -- Check if reference points to NULL
    emitAssOps [
        MOVL refMemoryLoc (Reg EAX),
    -- debug
        -- PUSH (Reg EAX),
        -- CALL libPrintInt,
        -- POP (Reg EAX),
    -- end of debug
        TEST (Reg EAX) (Reg EAX),
        JZ labelAfter
        ]
    decrRefCnt EAX labelAfter
    emitLabel labelAfter)

-- Assumes memory is not empty == != null
-- reg should store reference == address of object
decrRefCnt:: Register -> Label -> CompilerMonad ()
decrRefCnt reg labelAfter = do
    labelBegin <- getNewLabelCustom labelInfoDecrRefCntEntry 
    labelAfter' <- getNewLabelCustom labelInfoDecrRefCntEnd 
    emitLabel labelBegin
    emitAssOps [
        -- debug
        -- PUSH (Reg reg),
        -- LEAL (MemoryLocOffset reg refCounterOffset) (Reg ECX),
        -- PUSH (Reg ECX),
        -- CALL libPrintInt,
        -- POP (Reg ECX),
        -- POP (Reg reg),
        -- enddebug
        MOVL (Reg reg) (Reg EAX),                               -- object's address
        DECL (MemoryLocOffset reg refCounterOffset),            -- decl ref_counter
        MOVL (MemoryLocOffset reg refCounterOffset) (Reg ECX),  -- get current value
        
        -- --   debug
        -- PUSH (Reg ECX),
        -- PUSH (Reg EAX),
        -- CALL libPrintInt, -- prints current object's addr 
        -- POP (Reg EAX),
        -- POP (Reg ECX),
        -- -- enddebug


        -- --   debug
        -- PUSH (Reg EAX),
        -- PUSH (Reg ECX),
        -- CALL libPrintInt, -- prints current ref_cnt 
        -- POP (Reg ECX),
        -- POP (Reg EAX),
        -- -- enddebug


        TEST (Reg ECX) (Reg ECX),                               -- test if 0
        JNZ labelAfter
        ]
    
    emitAssOps [
        PUSH (Reg EAX),                                         -- push 'self'
        MOVL (MemoryLocOffset EAX destructorOffset) (Reg EAX),  -- get dtor offset
        CALLREG EAX,                                            -- call object's destructor
        ADDL (IntLit dWord) (Reg ESP)                           -- clean
        ]
    emitLabel labelAfter'





-----------------
-- 1. Destructor gets pointer to the the object/array/string on stack
-- 2. Memory layout:
-- [dtor, ref_cnt, 'this', field0, field1, ...]
-- [dtor, ref_cnt, arr.length, el0, el1, ...]
-- [dtor, ref_cnt, char0, char1, ...]
--                 ^ this is where variable we get is pointing to

-- Assumption: if constructor is called then the object under given address is NOT NULL, no need to check
setDestructor reg dtorLabel = emitAssOp $ MOVL (Label dtorLabel) (MemoryLocOffset reg destructorOffset)

setDefaultDestructor reg = do
    emitAssOp $ MOVL (Label defaultDestructorLabel) (MemoryLocOffset reg destructorOffset)

-- on top of the stack (first argument) should be pointer to object we want to free
emitDefaultDestructor = do
    emitAssOp NEWLINE
    emitAssOp $ FNLABEL defaultDestructorLabel
    emitAssOps [
        MOVL (MemoryLocOffset ESP dWord) (Reg EAX), -- extract first argument == 'self'
        -- debug
        -- PUSH (IntLit 42),
        -- CALL libPrintInt,
        -- ADDL (IntLit dWord) (Reg ESP),
        -- enddebug

        ADDL (IntLit destructorOffset) (Reg EAX),
        PUSH (Reg EAX),
        -- debug
        -- CALL libPrintInt,
        -- ADDL (IntLit metadataOffset) (MemoryLoc ESP), -- memory layout: [destructor, ref_count, 'self', ...]
        -- CALL libPrintInt,
        -- enddebug
        
        CALL "free",
        ADDL (IntLit dWord) (Reg ESP),
        RET -- no local variables, so just return
        ]


-- optimized version allocating EBX to be used as 'inner_for_loop_idx'
emitArrayComplexDestructor :: CompilerMonad ()
emitArrayComplexDestructor = do
    emitAssOp NEWLINE
    emitAssOp $ FNLABEL arrayComplexDestructorLabel
    labelBody <- getNewLabelCustom "array_dtor_for_loop"
    labelCond <- getNewLabelCustom "array_dtor_for_loop_cond"

    emitAssOp $ PUSH (Reg EBX)                    -- memorize old EBX
    emitAssOp $ MOVL (MemoryLocOffset ESP (2*dWord)) (Reg EAX) -- EAX stores MemoryAddress of 'self' ~ arr.length 
    emitLoopInit EAX
    emitAssOp $ JMP labelCond
    emitLabel labelBody
    emitLoopBody labelCond
    emitLabel labelCond
    emitForLoopCond labelBody
    emitAssOps [
        ADDL (IntLit dWord) (Reg ESP), -- "pop" array.length
        ADDL (IntLit destructorOffset) (MemoryLoc ESP), -- memory layout: [destructor, ref_count, 'self', ...]
        CALL "free",
        ADDL (IntLit dWord) (Reg ESP),
        POP (Reg EBX),          -- restore old EBX
        RET
        ]

    where
        emitLoopInit reg = emitAssOps [
            PUSH (Reg reg),                   -- pointer to array length
            PUSH (MemoryLoc reg),             -- array.length
            XORL (Reg EBX) (Reg EBX)]                  -- int i = 0;
        
        emitForLoopCond labelBody = do
            emitAssOps [
                INCL (Reg EBX),  -- i++
                MOVL (MemoryLoc ESP) (Reg EAX), -- array.length
                CMPL (Reg EAX) (Reg EBX), -- test i - array.length
                JLE labelBody]

        -- If object we store is not null, then decr it's ref_cnt. 
        -- This might lead to recursive call to destructor, if rec_cnt becomes 0
        emitLoopBody labelCond = do
            emitAssOps [
            ---- debug
                -- PUSH (IntLit 40),
                -- CALL libPrintInt,
                -- POP (Reg EAX),
                -- Set next object
            ---- enddebug
                MOVL (MemoryLocOffset ESP dWord) (Reg EAX),  -- %eax := address of pointer to the array ~ 'self'
            -- -- debug
                -- PUSH (Reg EAX),
                -- CALL libPrintInt,
                -- POP (Reg EAX),
            -- -- enddebug
                -- MOVL (MemoryLoc ESP) (Reg ECX),                  -- %ebx := idx
                MOVL (MemoryLocOffset2 EAX EBX dWord) (Reg EAX), -- %eax := eax + 4 * ebx = (array) + 4 * i
            ---- debug
                -- PUSH (Reg EAX),
                -- CALL libPrintInt,
                -- POP (Reg EAX),
            ---- enddebug
                -- MOVL (MemoryLocOffset2 EAX EBX dWord) (Reg EAX), -- %eax := eax + 4 * ebx = (array) + 4 * i
                -- MOVL (MemoryLoc EAX) (Reg EAX),                   -- object at arr[i] TODO perhaps I should check for null here?
                -- debug
                -- PUSH (Reg EAX),
                -- PUSH (IntLit 42),
                -- CALL libPrintInt,
                -- POP (Reg EAX),
                -- POP (Reg EAX),
                -- enddebug
                TEST (Reg EAX) (Reg EAX),   -- test if object is null 
                JZ labelCond                -- if not, free memory
                -- --debug
                -- PUSH (Reg EAX),
                -- PUSH (Reg ECX),
                -- CALL libPrintInt,
                -- POP (Reg ECX),
                -- POP (Reg EAX)
                -- --enddebug
                -- PUSH (Reg EAX),
                -- debug
                -- PUSH (Reg EAX),
                -- PUSH (IntLit 42),
                -- CALL libPrintInt,
                -- POP (Reg EAX),
                -- POP (Reg EAX),
                -- enddebug
                ]

            -- labelAfter <- getNewLabel
            decrRefCnt EAX labelCond
            -- emitAssOps[
            --     -- HERE DECREASE COUNTER DON"T FREE :< 

            --     -- free if exists -- TODO not free... decrease counter...
            --     PUSH (Reg EAX),                             -- push 'self' of the object in the array. Namely a[i]
            --     -- debug
            --     PUSH (Reg EAX),
            --     CALL libPrintInt,
            --     POP (Reg EAX),
            --     -- enddebug
            --     MOVL (MemoryLocOffset EAX destructorOffset) (Reg EAX),   -- get its dtor

            --     -- debug
            --     PUSH (Reg EAX),
            --     CALL libPrintInt,
            --     POP (Reg EAX),
            --     -- enddebug
            --     -- LEAL (MemoryLocOffset EAX destructorOffset) (Reg EAX),
            --     CALLREG EAX,
            --     ADDL (IntLit dWord) (Reg ESP)
            --     ]
            -- emitLabel labelAfter
    

-- Expects 'self' as a parameter
-- consider Class A {
--  string s;
--  int a;
--  int[] arr;
-- }
-- Memory layout:
-- [dtor, ref_cnt, 'self'=='vTable', s, a, arr]
--                  ^ given address of 'self'
-- Schema:
-- class_dtor (
    --     get 'self'
    --     for each field
    --        if of type {string, array, object} 
    --          loc <- field location
    --          if (loc) != 0 then call dctr for object under this loc
    --     free(self + dctrOffset)  
    -- )
emitClassDestructor :: ClassName -> [(VarName, (VarType, VarIndex))] -> CompilerMonad()
emitClassDestructor className classFields = do
    let complexFields = reverse $ filter (\(_, (type_, _)) -> isComplexType type_) classFields
    emitAssOp NEWLINE
    emitAssOp $ METHODLABEL $ getClassDtorLabel className
    emitAssOps [
        PUSH (Reg ESI),                               -- memorize old ESI
        MOVL (MemoryLocOffset ESP (2*dWord)) (Reg ESI)    -- Memorize 'self' in ESI
        ]
    -- return ()
    -- Free object's fields. In reversed order - like c++
    -- Invariant: ECX stores address of 'self'. *self == vTable
    -- emitAssOps [
        -- emitAssOps [PUSH (Reg EBP), MOVL (Reg ESP) (Reg EBP)]
        -- PUSH (Reg EBX)                                -- memorize original EBX
        -- ]
    forM_ (reverse complexFields) (\(_, (type_, ClassVar idx)) -> (do 
        labelAfter <- getNewLabelCustom "class_dtor_field_check_end"
        emitAssOps [
                -- MOVL (MemoryLocOffset ESP dWord) (Reg ECX),-- TODO be carefull here, but EBX is prevent for calls afaik.
                -- debug
                -- PUSH (Reg ECX),
                -- CALL libPrintInt,
                -- POP (Reg ECX),
                -- enddebug
                
                --debug
                -- PUSH (Reg EAX),
                -- PUSH (Reg ECX),
                -- LEAL (MemoryLocOffset ECX (dWord * (idx + 1))) (Reg EAX),
                -- PUSH (Reg EAX),
                -- CALL libPrintInt,
                -- POP (Reg EAX),
                -- POP (Reg ECX),
                -- POP (Reg EAX),
                -- enddebug
                
                MOVL (MemoryLocOffset ESI (dWord * (idx + 1))) (Reg EAX), -- load value, e.g. this->s 
                -- debug
                -- PUSH (Reg EAX),
                -- CALL libPrintInt,               -- current value of object's field ~ since it's reference thus it's memoryAddress
                -- POP (Reg EAX),
                -- enddebug
                TEST (Reg EAX) (Reg EAX),                                 -- test if non-null reference
                JZ labelAfter
            ]
        --         -- debug
        -- emitAssOps [ 
        --         PUSH (Reg EAX),
        --         PUSH (IntLit 101),
        --         CALL libPrintInt,
        --         ADDL (IntLit dWord) (Reg ESP),
        --         POP (Reg EAX)
        --     ]
                -- enddebug
        decrRefCnt EAX labelAfter
        emitLabel labelAfter
        ))
    
    -- free the actual object
    emitAssOps [
        -- MOVL (MemoryLocOffset ESP (dWord)) (Reg EAX),    -- get address of 'this'
        -- ADDL (IntLit destructorOffset) (Reg EAX), -- move to the beginning of the space
        -- PUSH (Reg EAX),
        -- ESI won't be needed anymore, thus we can modify it
        ADDL (IntLit destructorOffset) (Reg ESI), -- move to the beginning of the allocated space
        PUSH (Reg ESI),
        CALL "free",
        ADDL (IntLit dWord) (Reg ESP),
        POP (Reg ESI),                      -- restore original ESI
        RET
        ]
