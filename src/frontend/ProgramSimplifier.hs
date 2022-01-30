module ProgramSimplifier where

import Data.Int

import Common
import Utils
import AbsLatte

-- Note: this operation losses 'position' information
-- Perhaps it will be good to remove 'position' from types, but it can be done during SSA form generation.
-- Currently ProgramSimplifier offers simplification of constant expressions
simplifyProgram :: ProgramMeta -> ProgramMeta
simplifyProgram (Program _ topDefs) = Program Nothing (map simplifyTopDef topDefs)

simplifyTopDef :: TopDefMeta -> TopDefMeta
simplifyTopDef (TopClassDef _ (ClassDef _ className classMembers)) = TopClassDef Nothing (ClassDef Nothing className (map simplifyClassMember classMembers))
simplifyTopDef (TopClassDef _ (ClassExtDef _ className parentClass classMembers)) = TopClassDef Nothing (ClassExtDef Nothing className parentClass (map simplifyClassMember classMembers))
simplifyTopDef (TopFuncDef _ funcDef) = TopFuncDef Nothing (simplifyFuncDef funcDef)

simplifyClassMember :: ClassMemberMeta -> ClassMemberMeta
simplifyClassMember ret@ClassField{} = ret
simplifyClassMember (ClassMethod _ funcDef) = ClassMethod Nothing (simplifyFuncDef funcDef)

simplifyFuncDef :: FuncDefMeta -> FuncDefMeta
simplifyFuncDef (FuncDef _ retType funcName args (Block _ stmts)) = FuncDef Nothing retType funcName args (Block Nothing (map simplifyStmt stmts))

simplifyStmt :: StmtMeta -> StmtMeta
simplifyStmt stmt@(Empty _) = stmt
simplifyStmt (BStmt _ (Block _ stmts)) = BStmt Nothing (Block Nothing (map simplifyStmt stmts))
simplifyStmt (Decl _ type_ items) = Decl Nothing type_ (map simplifyItem items)
simplifyStmt (Ass _ expr1 expr2) = Ass Nothing (simplifyExpr expr1) (simplifyExpr expr2)
simplifyStmt (Incr _ expr) = Incr Nothing (simplifyExpr expr) 
simplifyStmt (Decr _ expr) = Decr Nothing (simplifyExpr expr) 
simplifyStmt (Ret _ expr) = Ret Nothing (simplifyExpr expr)
simplifyStmt stmt@(VRet _) = stmt 
simplifyStmt (Cond _ expr stmt) = Cond Nothing (simplifyExpr expr) (simplifyStmt stmt)
simplifyStmt (CondElse _ expr stmt1 stmt2) = CondElse Nothing (simplifyExpr expr) (simplifyStmt stmt1) (simplifyStmt stmt2)
simplifyStmt (While _ expr stmt) = While Nothing (simplifyExpr expr) (simplifyStmt stmt)
simplifyStmt (For _ type_ ident expr stmt) = For Nothing type_ ident (simplifyExpr expr) (simplifyStmt stmt)
simplifyStmt (SExp _ expr) = SExp Nothing (simplifyExpr expr)

simplifyItem :: ItemMeta -> ItemMeta
simplifyItem item@(NoInit _ _) = item
simplifyItem (Init _ ident expr) = Init Nothing ident (simplifyExpr expr)

simplifyExpr :: ExprMeta -> ExprMeta
simplifyExpr res@(ENewObject _ _) = res
simplifyExpr (ENewArray _ type_ expr) = ENewArray Nothing type_ (simplifyExpr expr)
simplifyExpr (EMember _ exprVar attributeIdent) = EMember Nothing (simplifyExpr exprVar) attributeIdent
simplifyExpr (EMemberCall _ varName methodName exprsArgs) = EMemberCall Nothing varName methodName (map simplifyExpr exprsArgs)
simplifyExpr res@(ELitInt _ _) = res
simplifyExpr res@(ELitTrue _) = res
simplifyExpr res@(ELitFalse _) = res
simplifyExpr (EString _ str) = EString Nothing (purifyString str)
simplifyExpr res@(EVar _ _) = res
simplifyExpr res@(ECast _ _) = res
simplifyExpr (EApp _ ident exprs) = EApp Nothing ident (map simplifyExpr exprs)
simplifyExpr (EArrGet _ exprVar exprIdx) = EArrGet Nothing (simplifyExpr exprVar) (simplifyExpr exprIdx)

simplifyExpr (Neg _ expr) = case simplifyExpr expr of
    (ELitInt _ val) -> ELitInt Nothing (unaryOpOnInt32 negate val)
    expr' -> Neg Nothing expr'

simplifyExpr (Not _ (Not _ expr)) = simplifyExpr expr
simplifyExpr original@(Not _ expr) = case simplifyExpr expr of
    (ELitTrue _) -> ELitFalse Nothing
    (ELitFalse _) -> ELitTrue Nothing
    Not _ expr -> expr
    EAnd _ expr1 expr2 -> simplifyExpr (EOr Nothing (Not Nothing expr1) (Not Nothing expr2)) 
    EOr _ expr1 expr2 -> simplifyExpr (EAnd Nothing (Not Nothing expr1) (Not Nothing expr2))
    ERel _ expr1 relOp expr2 -> simplifyExpr (ERel Nothing expr1 (negateRelOp relOp) expr2)
    _ -> original

    where
        negateRelOp :: RelOpMeta -> RelOpMeta
        negateRelOp (EQU p) = NE p
        negateRelOp (NE p) = EQU p
        negateRelOp (LTH p) = GE p
        negateRelOp (GE p) = LTH p
        negateRelOp (LE p) = GTH p
        negateRelOp (GTH p) = LE p


simplifyExpr original@(EMul _ expr1 mulOp expr2) = case mulOp of
    Times _ -> simplifyBinaryExprIfInt32 original (*) expr1 expr2
    Div _ -> simplifyBinaryExprIfInt32 original quot expr1 expr2
    Mod _ -> simplifyBinaryExprIfInt32 original rem expr1 expr2

simplifyExpr original@(EAdd _ expr1 addOp expr2) = case addOp of
    Plus _ -> case (simplifyExpr expr1, simplifyExpr expr2) of
        (EString _ str1, EString _ str2) -> EString Nothing (purifyString str1 ++ purifyString str2)
        (ELitInt _ val1, ELitInt _ val2) -> ELitInt Nothing (binaryOpOnInt32 (+) val1 val2)
        _ -> original
    Minus _ -> simplifyBinaryExprIfInt32 original (-) expr1 expr2

-- EQU and NE are defined for Int, String and Null. Other operators works only on Int
-- If not constant, then it will have to be evaluated during runtime
simplifyExpr original@(ERel _ expr1 relOp expr2) = let (leftExpr, rightExpr) = (simplifyExpr expr1, simplifyExpr expr2) in 
    let newOriginal = ERel Nothing leftExpr relOp rightExpr in case relOp of
        (EQU _) -> case (leftExpr, rightExpr) of
            (ECast _ _, ECast _ _) -> boolToExpr True
            (ELitTrue _, ELitTrue _) -> boolToExpr True
            (ELitFalse _, ELitFalse _) -> boolToExpr True
            (ELitFalse _, ELitTrue _) -> boolToExpr False
            (ELitTrue _, ELitFalse _) -> boolToExpr False
            (ELitInt _ val1, ELitInt _ val2) -> boolToExpr (val1 == val2)
            (EString _ val1, EString _ val2) -> boolToExpr (purifyString val1 == purifyString val2)
            _ -> newOriginal
        (NE _) -> case (leftExpr, rightExpr) of
            (ECast _ _, ECast _ _) -> boolToExpr False
            (ELitTrue _, ELitTrue _) -> boolToExpr False
            (ELitFalse _, ELitFalse _) -> boolToExpr False
            (ELitFalse _, ELitTrue _) -> boolToExpr True
            (ELitTrue _, ELitFalse _) -> boolToExpr True
            (ELitInt _ val1, ELitInt _ val2) -> boolToExpr (val1 /= val2)
            (EString _ val1, EString _ val2) -> boolToExpr (purifyString val1 /= purifyString val2)
            _ -> newOriginal
        _ -> let op = unpackRelOpExpr relOp in case (leftExpr, rightExpr) of
            (ELitInt _ val1, ELitInt _ val2) -> boolToExpr (op val1 val2)
            _ -> newOriginal
        
        where
            unpackRelOpExpr (LTH _) = (<)
            unpackRelOpExpr (LE _) = (<=)
            unpackRelOpExpr (GTH _) = (>)
            unpackRelOpExpr (GE _) = (>=)
            unpackRelOpExpr (NE _) = (/=)
            unpackRelOpExpr (EQU _) = (==)

simplifyExpr (EAnd _ expr1 expr2) = 
    let rightExpr = simplifyExpr expr2 in
        case simplifyExpr expr1 of
            ELitTrue _ -> rightExpr
            ELitFalse _ -> ELitFalse Nothing
            leftExpr -> EAnd Nothing leftExpr rightExpr

simplifyExpr (EOr _ expr1 expr2) = 
    let rightExpr = simplifyExpr expr2 in
        case simplifyExpr expr1 of
            ELitTrue _ -> ELitTrue Nothing
            ELitFalse _ -> rightExpr
            leftExpr -> EOr Nothing leftExpr rightExpr



------------------------------ Utils ------------------------------

-- Takes operator and expresions expr1, expr2 and originalExpr for expr1, expr2. Simplifies them, and 
-- if both result in ELitInt then applies operator on values casted to 32bit integers
-- otherwise returns originalExpr
simplifyBinaryExprIfInt32 :: ExprMeta -> (Int32 -> Int32 -> Int32) -> ExprMeta -> ExprMeta -> ExprMeta
simplifyBinaryExprIfInt32 originalExpr op exprLeft exprRight = case (simplifyExpr exprLeft, simplifyExpr exprRight) of
    (ELitInt _ val1, ELitInt _ val2) -> ELitInt Nothing (binaryOpOnInt32 op val1 val2)
    _ -> originalExpr

-- Takes operator and Integer a and applies operator on a casted to 32bit integer
unaryOpOnInt32 :: (Int32 -> Int32) -> Integer -> Integer
unaryOpOnInt32 op val = toInteger (op (toInt32 val))

-- Takes operator and Integers a,b and applies operator on a,b casted to 32bit integers
binaryOpOnInt32 :: (Int32 -> Int32 -> Int32) -> Integer -> Integer -> Integer
binaryOpOnInt32 op val1 val2 = let (val1', val2') = (toInt32 val1, toInt32 val2) in toInteger (op val1' val2')

toInt32 :: Integer -> Int32
toInt32 = fromIntegral

-- Packs bool value into expression
boolToExpr :: Bool -> ExprMeta
boolToExpr True = ELitTrue Nothing
boolToExpr False = ELitFalse Nothing

