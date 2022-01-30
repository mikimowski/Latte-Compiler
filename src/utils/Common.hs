module Common where

import AbsLatte
import OrderedMap
import qualified Data.Map as Map

---------------- AbsLatte ------------------  
type Position = Maybe (Int, Int)

type TopDefMeta = (TopDef Position)
type FuncDefMeta = (FuncDef Position)
type ClassDefMeta = (ClassDef Position)
type ClassMemberMeta = (ClassMember Position)
type TypeMeta = (AbsLatte.Type Position)
type ExprMeta = (Expr Position)
type ArgMeta = (Arg Position)
type BlockMeta = (Block Position)
type RelOpMeta = (RelOp Position)
type MulOpMeta = (MulOp Position)
type AddOpMeta = (AddOp Position)
type StmtMeta = (Stmt Position)
type ProgramMeta = (Program Position)
type ItemMeta = (Item Position)

isArrayType :: TypeMeta -> Bool
isArrayType (TArray _ type_) = case type_ of
    (TVoid _) -> False
    _ -> True
isArrayType _ = False

isVoidType :: TypeMeta -> Bool
isVoidType (TVoid _) = True
isVoidType _ = False

isIntType :: TypeMeta -> Bool
isIntType (TInt _) = True
isIntType _ = False

isBoolType :: TypeMeta -> Bool
isBoolType (TBool _) = True
isBoolType _ = False

isComplexType :: TypeMeta -> Bool
isComplexType TClass {} = True
isComplexType TArray {} = True
isComplexType TStr {} = True
isComplexType _ = False

filterTopFuncDef = filter (\topDef -> case topDef of
    (TopFuncDef _ _) -> True
    _ -> False
    )

filterClassFields = filter (\classMember -> case classMember of 
        (ClassField _ _ _) -> True
        _ -> False
        ) 

filterClassMethods = filter (\classMember -> case classMember of 
        (ClassMethod _ _) -> True
        _ -> False
        )

---------------- Variables ------------------  
type VarType = TypeMeta
type VarName = Ident

---------------- Functions ------------------  
type FuncName = Ident
type FuncType = (VarType, [VarType])

---------------- Classes ------------------  
type ClassName = Ident
type MemberName = Ident
type MemberSource = ClassName
type ClassMembers v = OrderedMap MemberName v
type ClassFields = ClassMembers (Int, VarType, MemberSource)
type ClassMethods = ClassMembers (Int, FuncType, MemberSource)

data ClassMetadata = ClassMetadata {
    name :: ClassName,
    parentClass :: Maybe ClassName,
    fields :: ClassFields, 
    methods :: ClassMethods
}

---------------- Frontend ------------------ 
type FunctionsTypes = Map.Map FuncName FuncType
type ClassesTypes = Map.Map ClassName ClassMetadata 
type FrontendResult = (ProgramMeta, FunctionsTypes, ClassesTypes)

---------------- Builts-in ------------------ 
typeVoid = TVoid Nothing
typeInt = TInt Nothing
typeBool = TBool Nothing
typeString = TStr Nothing
typeClassJoker = TClass Nothing (Ident "__internal_joker_class")

----- functions -----
printIntBuiltInFunc = (Ident "printInt", typeVoid, [TInt Nothing])
printStringBuiltInFunc = (Ident "printString", typeVoid, [TStr Nothing])
errorBuiltInFunc = (Ident "error", typeVoid, [])
readIntBuiltInFunc = (Ident "readInt", typeInt, [])
readStringBuiltInFunc = (Ident "readString", typeString, [])
builtInFunctions = [readStringBuiltInFunc, readIntBuiltInFunc, errorBuiltInFunc, printStringBuiltInFunc, printIntBuiltInFunc]


buildFuncType :: FuncDefMeta -> FuncType
buildFuncType (FuncDef _ retType _ args _) =
  let argsTypes = map (\(Arg _ argType _) -> argType) args in (retType, argsTypes)

---------------- Language features ------------------ 
mainFunctionLabel = "main" 
arrayLengthAttribute = Ident "length"
classSelfVariable = Ident "self"
mainFunctionIdent = Ident mainFunctionLabel
validMainFunctionType = (typeInt, [])
isValidMainFunctionType type_ = type_ == validMainFunctionType

---------------- Backend ----------------
type Assemblyx86Code = String
type Assemblyx86Program = [Assemblyx86Code]
type Label = String