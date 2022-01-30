{-# LANGUAGE GADTs #-}

module Assemblyx86 where

import Common

data Register = EAX | EBX | ECX | EDX | EBP | ESP | ESI | EDI
    deriving Eq

data Operand = 
      Label String
    | IntLit Int
    | Reg Register
    | MemoryLoc Register
    | MemoryLocOffset Register Int
    | MemoryLocOffset2 Register Register Int

data AssOp where
    MOVL :: Operand -> Operand -> AssOp
    CMPL :: Operand -> Operand -> AssOp -- test (a-b)
    TEST :: Operand -> Operand -> AssOp
    ADDL :: Operand -> Operand -> AssOp
    SUBL :: Operand -> Operand -> AssOp
    IMUL :: Operand -> Operand -> AssOp
    XORL :: Operand -> Operand -> AssOp
    LEAL :: Operand -> Operand -> AssOp

    PUSH :: Operand -> AssOp
    POP :: Operand -> AssOp
    NEGL :: Operand -> AssOp
    NOT :: Operand -> AssOp
    INCL :: Operand -> AssOp
    DECL :: Operand -> AssOp
    IDIVL :: Operand -> AssOp
    
    JE :: String -> AssOp
    JNE :: String -> AssOp
    JGE :: String -> AssOp
    JG :: String -> AssOp
    JLE :: String -> AssOp
    JL :: String -> AssOp
    JMP :: String -> AssOp
    JZ :: String -> AssOp
    JNZ :: String -> AssOp
    
    CALL :: String -> AssOp
    CALLREG :: Register -> AssOp

    RET :: AssOp
    CLTD :: AssOp
    LEAVE :: AssOp   -- equivalent to %ebp, %esp; popl %ebp

    LABEL :: String -> AssOp
    FNLABEL :: String -> AssOp
    METHODLABEL :: String -> AssOp
    NEWLINE :: AssOp -- custom utility, estetic one

instance Show Register where
    show EAX = "%eax"
    show EBX = "%ebx"
    show ECX = "%ecx"
    show EDX = "%edx"
    show EBP = "%ebp"
    show ESP = "%esp"
    show ESI = "%esi"
    show EDI = "%edi"

instance Show Operand where
    show (Label label) = "$" ++ label
    show (IntLit val) = "$" ++ show val
    show (Reg reg) = show reg
    show (MemoryLoc reg) = "(" ++ show reg ++ ")"
    show (MemoryLocOffset reg offset) = show offset ++ "(" ++ show reg ++ ")"
    show (MemoryLocOffset2 reg1 reg2 offset) = "(" ++ show reg1 ++ "," ++ show reg2 ++ "," ++ show offset ++ ")"

instance Show AssOp where
    -- Binary ops
    show (MOVL a b) = show2 "movl" a b
    show (CMPL a b) = show2 "cmpl" a b
    show (TEST a b) = show2 "test" a b
    show (ADDL a b) = show2 "addl" a b
    show (SUBL a b) = show2 "subl" a b
    show (IMUL a b) = show2 "imul" a b
    show (XORL a b) = show2 "xorl" a b
    show (LEAL a b) = show2 "leal" a b

    -- Unary ops
    show (PUSH a) = show1 "push" a
    show (POP a) = show1 "pop" a
    show (NEGL a) = show1 "negl" a
    show (NOT a) = show1 "not" a
    show (INCL a) = show1 "incl" a
    show (DECL a) = show1 "decl" a
    show (IDIVL a) = show1 "idivl" a

    show (JMP label) = "jmp " ++ label
    show (JZ label) = "jz " ++ label
    show (JNZ label) = "jnz " ++ label
    show (JE label) = "je " ++ label
    show (JNE label) = "jne " ++ label
    show (JGE label) = "jge " ++ label
    show (JG label) = "jg " ++ label
    show (JLE label) = "jle " ++ label
    show (JL label) = "jl " ++ label

    show (CALL f) = "call " ++ f
    show (CALLREG reg) = "call *" ++ show reg

    show CLTD = "cltd"
    show RET = "ret"
    show LEAVE = "leave"

    show NEWLINE = ""

show1 op arg = op ++ " " ++ show arg
show2 op arg1 arg2 = op ++ " " ++ show arg1 ++ ", " ++ show arg2

convertToProgram :: [AssOp] -> [Assemblyx86Code]
convertToProgram = map (\op -> case op of
    (LABEL lbl) -> lbl ++ ":"
    (FNLABEL lbl) -> lbl ++ ":"
    (METHODLABEL lbl) -> lbl ++ ":"
    op -> ((indent ++) . show) op)
    where 
        indent = "    "