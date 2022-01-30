module StaticError where

import Text.Printf
import Data.List

import Common
import AbsLatte


data StaticErrorType = 
                          FunctionRedeclaration Ident
                        | VariableRedeclaration VarName
                        | ClassRedeclaration ClassName
                        | VoidArgumentType Ident TypeMeta Ident
                        | InvalidType TypeMeta
                        | DuplicateFunctionArgument Ident Ident
                        | UndefinedClassInheritance Ident Ident
                        | CycleInheritance [ClassName]
                        | DuplicateAttribute Ident ClassName
                        | DuplicateMethod Ident ClassName
                        | InheritanceAttributeTypeMismatch Ident TypeMeta ClassName TypeMeta ClassName     
                        | InheritanceMethodTypeMismatch Ident FuncType ClassName FuncType ClassName
                        | TypeNotDeclared TypeMeta
                        | IntegerLiteralOutOfBounds Integer
                        | ClassNotDefined ClassName
                        | InvalidTypeInitialization TypeMeta TypeMeta
                        | InvalidTypeAssignment TypeMeta TypeMeta
                        | InvalidTypeIncrementation TypeMeta
                        | InvalidTypeIncrementationArrayLength
                        | InvalidTypeDecrementation TypeMeta
                        | InvalidTypeDecrementationArrayLength
                        | InvalidTypeArithmeticNegation TypeMeta
                        | InvalidTypeBooleanNegation TypeMeta
                        | InvalidTypeMultiplication TypeMeta TypeMeta
                        | InvalidTypeDivision TypeMeta TypeMeta
                        | InvalidTypeModulo TypeMeta TypeMeta
                        | InvalidTypeAddition TypeMeta TypeMeta
                        | InvalidTypeSubtraction TypeMeta TypeMeta
                        | InvalidTypeComparison TypeMeta TypeMeta
                        | InvalidTypeBooleanOr TypeMeta TypeMeta
                        | InvalidTypeBooleanAnd TypeMeta TypeMeta
                        | InvalidTypeArrayGet TypeMeta
                        | InvalidTypeForLoopContainer TypeMeta
                        | InvalidTypeForLoopVariable TypeMeta TypeMeta
                        | InvalidTypeMemberAccessNotAClass TypeMeta
                        | InvalidTypeCondition TypeMeta
                        | ClassMemberNotExist Ident Ident
                        | InvalidNumberOfArguments Int Int
                        | ArgumentTypeMismatch Int TypeMeta TypeMeta
                        | InvalidReturnType TypeMeta TypeMeta
                        | InvalidReturnTypeVoid
                        | IndexTypeNotInteger TypeMeta
                        | SizeTypeNotInteger TypeMeta
                        | VariableNotDefined Ident
                        | FunctionNotDefined Ident
                        | UnknownArrayAttribute Ident
                        | MissingReturn Ident
                        | InvalidMainFunctionType FuncType FuncType
                        | InvalidTypeVariableDeclaration TypeMeta
                        | InvalidLValue 
                        | InvalidLValueArrayLength
                        | SelfClassKeywordOverride String
                        | SelfClassKeywordAssignment 
                        | SelfClassKeyword String

                        | Other String


instance Show StaticErrorType where
    show (FunctionRedeclaration (Ident funcName)) = printf "Function '%s' already defined" funcName 
    show (VariableRedeclaration (Ident varName)) = printf "Variable '%s' already defined" varName
    show (VoidArgumentType (Ident argName) type_ (Ident funcName)) = printf "Invalid argument type '%s' of argument '%s' in function '%s'" (show type_) argName funcName 
    show (InvalidType type_) = printf "Invalid type '%s'" (show type_) 
    show (DuplicateFunctionArgument (Ident argName) (Ident funcName)) = printf "Duplicated function argument:\n    argument: '%s'\n    in function: '%s'" argName funcName
    show (UndefinedClassInheritance (Ident className) (Ident parentClassName)) = printf "Class '%s' extends undefined class '%s'" className parentClassName
    show (CycleInheritance cycle) = printf "Cycle inheritance: %s" $ intercalate " -> " (map (\(Ident className) -> show className) cycle)
    show (DuplicateAttribute (Ident attributeName) (Ident className)) = printf "Duplicate attribute '%s' in class '%s'" attributeName className 
    show (DuplicateMethod (Ident methodName) (Ident className)) = printf "Duplicate method '%s' in class '%s'" methodName className 
    show (InheritanceAttributeTypeMismatch (Ident attributeName) typeFound (Ident className) typeExpected (Ident sourceClassName)) = printf "Inherited attribute type mismatch:\n    found attribute: '%s' of type '%s' in class '%s',\n    expected type: '%s' inherited from class '%s'" attributeName (show typeFound) className (show typeExpected) sourceClassName
    show (InheritanceMethodTypeMismatch (Ident methodName) typeFound (Ident className) typeExpected (Ident sourceClassName)) = printf "Inherited method type mismatch:\n    found function: '%s' of type '%s' in class '%s',\n    expected type: '%s' inherited from class '%s'" methodName (show typeFound) className (show typeExpected) sourceClassName
    show (TypeNotDeclared type_) = printf "Type not declared:\n    type: '%s'" (show type_)
    show (IntegerLiteralOutOfBounds val) = printf "Integer literal out of bounds:\n    value: '%s'" (show val)
    show (ClassNotDefined className) = printf "Class not defined: '%s'" (show className)
    show (ClassRedeclaration className) = printf "Class redeclaration: '%s'" (show className)
    show (InvalidTypeInitialization actualType expectedType) = printf "Invalid type in initialization:\n    got: '%s'\n    expected: '%s'" (show actualType) (show expectedType)
    show (InvalidTypeAssignment actualType expectedType) = printf "Invalid type in assignment:\n    got: '%s'\n    expected: '%s'" (show actualType) (show expectedType)
    show (InvalidTypeDecrementation actualType) = printf "Invalid type for decrementation:\n    got: '%s'\n    expected: '%s'" (show actualType) (show (TInt Nothing))
    show InvalidTypeDecrementationArrayLength = printf "Invalid decrementation: cannot decrement array[]'s length"
    show (InvalidTypeIncrementation actualType) = printf "Invalid type for incrementation:\n    got: '%s'\n    expected: '%s'" (show actualType) (show (TInt Nothing))
    show InvalidTypeIncrementationArrayLength = printf "Invalid incrementation: cannot increment array[]'s length"
    show (InvalidTypeArithmeticNegation actualType) = printf "Invalid type for arithmetic negation:\n    got: '%s'\n    expected: '%s'" (show actualType) (show (TInt Nothing))
    show (InvalidTypeBooleanNegation actualType) = printf "Invalid type for boolean negation:\n    got: '%s'\n    expected: '%s'" (show actualType) (show (TBool Nothing))
    show (InvalidTypeMultiplication type1 type2) = printf "Multiplication not defined for types:\n    ('%s', '%s')" (show type1) (show type2) 
    show (InvalidTypeDivision type1 type2) = printf "Division not defined for types:\n    ('%s', '%s')" (show type1) (show type2) 
    show (InvalidTypeModulo type1 type2) = printf "Modulo not defined for types:\n    ('%s', '%s')" (show type1) (show type2) 
    show (InvalidTypeAddition type1 type2) = printf "Addition not defined for types:\n    ('%s', '%s')" (show type1) (show type2) 
    show (InvalidTypeSubtraction type1 type2) = printf "Subtraction not defined for types:\n    ('%s', '%s')" (show type1) (show type2) 
    show (InvalidTypeComparison type1 type2) = printf "Comparison not defined for types:\n    ('%s', '%s')" (show type1) (show type2) 
    show (InvalidTypeBooleanOr type1 type2) = printf "Boolean 'or' not defined for types:\n    ('%s', '%s')" (show type1) (show type2) 
    show (InvalidTypeBooleanAnd type1 type2) = printf "Boolean 'and' not defined for types:\n    ('%s', '%s')" (show type1) (show type2) 
    show (InvalidTypeArrayGet actualType) = printf "Invalid type in array indexing:\n    got: '%s'\n    expected: array type" (show actualType)
    show (InvalidTypeForLoopContainer actualType) = printf "Invalid type in for loop:\n    got: '%s'\n    expected: array type" (show actualType)
    show (InvalidTypeForLoopVariable actualType expectedType) = printf "Invalid type for for loop variable:\n    got: '%s'\n    expected: '%s'" (show actualType) (show expectedType)
    show (InvalidTypeMemberAccessNotAClass actualType) = printf "Invalid type in member access:\n    got: '%s'\n    expected: class type" (show actualType)
    show (InvalidTypeCondition actualType) = printf "Invalid type in boolean condition:\n    got: '%s'\n    expected: '%s'" (show actualType) (show (TBool Nothing))
    show (InvalidNumberOfArguments actualLen expectedLen) = printf "Invalid number of arguments in function call:\n    got: %s\n    expected: %s" (show actualLen) (show expectedLen)
    show (ArgumentTypeMismatch idx actualType expectedType) = printf "Argument type mismatch in function call:\n    at idx: #%s\n      got: '%s'\n      expected: '%s'" (show idx) (show actualType) (show expectedType)
    show (ClassMemberNotExist className memberName) = printf "Class's member not exists\n    for class: '%s'\n    for member: '%s'" (show className) (show memberName)
    show (InvalidReturnType actualType expectedType) = printf "Invalid return type:\n    got type: '%s'\n    expected: '%s'" (show actualType) (show expectedType)
    show InvalidReturnTypeVoid = printf "Invalid return type: '%s'" (show (TVoid Nothing))
    show (IndexTypeNotInteger actualType) = printf "Index type not integer:\n    got type: '%s'" (show actualType)
    show (SizeTypeNotInteger actualType) = printf "Size type not integer:\n    got type: '%s'" (show actualType)
    show (VariableNotDefined varName) = printf "Variable not defined: '%s'" (show varName)
    show (FunctionNotDefined funcName) = printf "Function not defined: '%s'" (show funcName)
    show (MissingReturn funcName) = printf "Missing return in function: '%s'" (show funcName)
    show (InvalidMainFunctionType actualType expectedType) = printf "Invalid main function type:\n    got type: '%s'\n    expected: '%s'" (show actualType) (show expectedType) 
    show (InvalidTypeVariableDeclaration actualType) = printf "Invalid type for variable declaration:\n    got type: '%s'" (show actualType) 
    show (UnknownArrayAttribute attributeName) = printf "Unknown array attribute: '%s'" (show attributeName)
    show InvalidLValue = printf "Invalid LValue:\n    left side of the assignment must be a valid lvalue" 
    show InvalidLValueArrayLength = printf "Invalid LValue:\n    left side of the assignment must be a valid lvalue, 'length' attribute of type array[] is not a valid lvalue" 
    show (SelfClassKeywordOverride msg) = printf "Class's '%s' keyword cannot be used as %s name" (show classSelfVariable) msg
    show SelfClassKeywordAssignment = printf "Class's '%s' keyword cannot be reassigned" (show classSelfVariable)
    show (SelfClassKeyword msg) = printf "Class's '%s' keyword cannot be used as %s" (show classSelfVariable) msg
    show (Other msg) = printf msg


data StaticError = StaticError {
    position :: Position,
    info :: StaticErrorType
}


instance Show StaticError where
    show error = case position error of
        (Just (line, column)) -> "StaticError at line " ++ show line ++ ", column = " ++ show column ++ ":\n  " ++ show err
        Nothing -> show err 
        where
            err = info error
    