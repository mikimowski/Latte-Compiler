-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParLatte where
import AbsLatte
import LexLatte
import ErrM

}

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%name pProgram_internal Program
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&&' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '+' { PT _ (TS _ 8) }
  '++' { PT _ (TS _ 9) }
  ',' { PT _ (TS _ 10) }
  '-' { PT _ (TS _ 11) }
  '--' { PT _ (TS _ 12) }
  '.' { PT _ (TS _ 13) }
  '/' { PT _ (TS _ 14) }
  ':' { PT _ (TS _ 15) }
  ';' { PT _ (TS _ 16) }
  '<' { PT _ (TS _ 17) }
  '<=' { PT _ (TS _ 18) }
  '=' { PT _ (TS _ 19) }
  '==' { PT _ (TS _ 20) }
  '>' { PT _ (TS _ 21) }
  '>=' { PT _ (TS _ 22) }
  '[' { PT _ (TS _ 23) }
  '[]' { PT _ (TS _ 24) }
  ']' { PT _ (TS _ 25) }
  'boolean' { PT _ (TS _ 26) }
  'cast' { PT _ (TS _ 27) }
  'class' { PT _ (TS _ 28) }
  'else' { PT _ (TS _ 29) }
  'extends' { PT _ (TS _ 30) }
  'false' { PT _ (TS _ 31) }
  'for' { PT _ (TS _ 32) }
  'if' { PT _ (TS _ 33) }
  'int' { PT _ (TS _ 34) }
  'new' { PT _ (TS _ 35) }
  'null' { PT _ (TS _ 36) }
  'return' { PT _ (TS _ 37) }
  'string' { PT _ (TS _ 38) }
  'true' { PT _ (TS _ 39) }
  'void' { PT _ (TS _ 40) }
  'while' { PT _ (TS _ 41) }
  '{' { PT _ (TS _ 42) }
  '||' { PT _ (TS _ 43) }
  '}' { PT _ (TS _ 44) }

  L_ident {PT _ (TV _)}
  L_integ {PT _ (TI _)}
  L_quoted {PT _ (TL _)}

%%

Ident :: {
  (Maybe (Int, Int), Ident)
}
: L_ident {
  (Just (tokenLineCol $1), Ident (prToken $1)) 
}

Integer :: {
  (Maybe (Int, Int), Integer)
}
: L_integ {
  (Just (tokenLineCol $1), read (prToken $1)) 
}

String :: {
  (Maybe (Int, Int), String)
}
: L_quoted {
  (Just (tokenLineCol $1), prToken $1)
}

Program :: {
  (Maybe (Int, Int), Program (Maybe (Int, Int)))
}
: ListTopDef {
  (fst $1, AbsLatte.Program (fst $1)(snd $1)) 
}

TopDef :: {
  (Maybe (Int, Int), TopDef (Maybe (Int, Int)))
}
: ClassDef {
  (fst $1, AbsLatte.TopClassDef (fst $1)(snd $1)) 
}
| FuncDef {
  (fst $1, AbsLatte.TopFuncDef (fst $1)(snd $1)) 
}

FuncDef :: {
  (Maybe (Int, Int), FuncDef (Maybe (Int, Int)))
}
: Type Ident '(' ListArg ')' Block {
  (fst $1, AbsLatte.FuncDef (fst $1)(snd $1)(snd $2)(snd $4)(snd $6)) 
}

Arg :: {
  (Maybe (Int, Int), Arg (Maybe (Int, Int)))
}
: Type Ident {
  (fst $1, AbsLatte.Arg (fst $1)(snd $1)(snd $2)) 
}

ClassMember :: {
  (Maybe (Int, Int), ClassMember (Maybe (Int, Int)))
}
: Type Ident ';' {
  (fst $1, AbsLatte.ClassField (fst $1)(snd $1)(snd $2)) 
}
| FuncDef {
  (fst $1, AbsLatte.ClassMethod (fst $1)(snd $1)) 
}

ClassDef :: {
  (Maybe (Int, Int), ClassDef (Maybe (Int, Int)))
}
: 'class' Ident '{' ListClassMember '}' {
  (Just (tokenLineCol $1), AbsLatte.ClassDef (Just (tokenLineCol $1)) (snd $2)(reverse (snd $4)))
}
| 'class' Ident 'extends' Ident '{' ListClassMember '}' {
  (Just (tokenLineCol $1), AbsLatte.ClassExtDef (Just (tokenLineCol $1)) (snd $2)(snd $4)(reverse (snd $6)))
}

ListArg :: {
  (Maybe (Int, Int), [Arg (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| Arg {
  (fst $1, (:[]) (snd $1)) 
}
| Arg ',' ListArg {
  (fst $1, (:) (snd $1)(snd $3)) 
}

ListTopDef :: {
  (Maybe (Int, Int), [TopDef (Maybe (Int, Int))]) 
}
: TopDef {
  (fst $1, (:[]) (snd $1)) 
}
| TopDef ListTopDef {
  (fst $1, (:) (snd $1)(snd $2)) 
}

ListClassMember :: {
  (Maybe (Int, Int), [ClassMember (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| ListClassMember ClassMember {
  (fst $1, flip (:) (snd $1)(snd $2)) 
}

Block :: {
  (Maybe (Int, Int), Block (Maybe (Int, Int)))
}
: '{' ListStmt '}' {
  (Just (tokenLineCol $1), AbsLatte.Block (Just (tokenLineCol $1)) (reverse (snd $2)))
}

Stmt :: {
  (Maybe (Int, Int), Stmt (Maybe (Int, Int)))
}
: ';' {
  (Just (tokenLineCol $1), AbsLatte.Empty (Just (tokenLineCol $1)))
}
| Block {
  (fst $1, AbsLatte.BStmt (fst $1)(snd $1)) 
}
| Type ListItem ';' {
  (fst $1, AbsLatte.Decl (fst $1)(snd $1)(snd $2)) 
}
| Expr '=' Expr ';' {
  (fst $1, AbsLatte.Ass (fst $1)(snd $1)(snd $3)) 
}
| Expr '++' ';' {
  (fst $1, AbsLatte.Incr (fst $1)(snd $1)) 
}
| Expr '--' ';' {
  (fst $1, AbsLatte.Decr (fst $1)(snd $1)) 
}
| 'return' Expr ';' {
  (Just (tokenLineCol $1), AbsLatte.Ret (Just (tokenLineCol $1)) (snd $2)) 
}
| 'return' ';' {
  (Just (tokenLineCol $1), AbsLatte.VRet (Just (tokenLineCol $1)))
}
| 'if' '(' Expr ')' Stmt {
  (Just (tokenLineCol $1), AbsLatte.Cond (Just (tokenLineCol $1)) (snd $3)(snd $5)) 
}
| 'if' '(' Expr ')' Stmt 'else' Stmt {
  (Just (tokenLineCol $1), AbsLatte.CondElse (Just (tokenLineCol $1)) (snd $3)(snd $5)(snd $7)) 
}
| 'while' '(' Expr ')' Stmt {
  (Just (tokenLineCol $1), AbsLatte.While (Just (tokenLineCol $1)) (snd $3)(snd $5)) 
}
| 'for' '(' Type Ident ':' Expr ')' Stmt {
  (Just (tokenLineCol $1), AbsLatte.For (Just (tokenLineCol $1)) (snd $3)(snd $4)(snd $6)(snd $8)) 
}
| Expr ';' {
  (fst $1, AbsLatte.SExp (fst $1)(snd $1)) 
}

Item :: {
  (Maybe (Int, Int), Item (Maybe (Int, Int)))
}
: Ident {
  (fst $1, AbsLatte.NoInit (fst $1)(snd $1)) 
}
| Ident '=' Expr {
  (fst $1, AbsLatte.Init (fst $1)(snd $1)(snd $3)) 
}

ListStmt :: {
  (Maybe (Int, Int), [Stmt (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| ListStmt Stmt {
  (fst $1, flip (:) (snd $1)(snd $2)) 
}

ListItem :: {
  (Maybe (Int, Int), [Item (Maybe (Int, Int))]) 
}
: Item {
  (fst $1, (:[]) (snd $1)) 
}
| Item ',' ListItem {
  (fst $1, (:) (snd $1)(snd $3)) 
}

Type :: {
  (Maybe (Int, Int), Type (Maybe (Int, Int)))
}
: Ident {
  (fst $1, AbsLatte.TClass (fst $1)(snd $1)) 
}
| 'int' {
  (Just (tokenLineCol $1), AbsLatte.TInt (Just (tokenLineCol $1)))
}
| 'string' {
  (Just (tokenLineCol $1), AbsLatte.TStr (Just (tokenLineCol $1)))
}
| 'boolean' {
  (Just (tokenLineCol $1), AbsLatte.TBool (Just (tokenLineCol $1)))
}
| 'void' {
  (Just (tokenLineCol $1), AbsLatte.TVoid (Just (tokenLineCol $1)))
}
| Type '[]' {
  (fst $1, AbsLatte.TArray (fst $1)(snd $1)) 
}

Expr6 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: 'new' Type {
  (Just (tokenLineCol $1), AbsLatte.ENewObject (Just (tokenLineCol $1)) (snd $2)) 
}
| 'new' Type '[' Expr ']' {
  (Just (tokenLineCol $1), AbsLatte.ENewArray (Just (tokenLineCol $1)) (snd $2)(snd $4)) 
}
| Expr6 '.' Ident {
  (fst $1, AbsLatte.EMember (fst $1)(snd $1)(snd $3)) 
}
| Expr6 '.' Ident '(' ListExpr ')' {
  (fst $1, AbsLatte.EMemberCall (fst $1)(snd $1)(snd $3)(snd $5)) 
}
| Ident {
  (fst $1, AbsLatte.EVar (fst $1)(snd $1)) 
}
| Integer {
  (fst $1, AbsLatte.ELitInt (fst $1)(snd $1)) 
}
| 'true' {
  (Just (tokenLineCol $1), AbsLatte.ELitTrue (Just (tokenLineCol $1)))
}
| 'false' {
  (Just (tokenLineCol $1), AbsLatte.ELitFalse (Just (tokenLineCol $1)))
}
| String {
  (fst $1, AbsLatte.EString (fst $1)(snd $1)) 
}
| Ident '(' ListExpr ')' {
  (fst $1, AbsLatte.EApp (fst $1)(snd $1)(snd $3)) 
}
| Expr6 '[' Expr ']' {
  (fst $1, AbsLatte.EArrGet (fst $1)(snd $1)(snd $3)) 
}
| '(' Expr ')' {
  (Just (tokenLineCol $1), snd $2)
}

Expr5 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: 'cast' '<' Ident ',' 'null' '>' {
  (Just (tokenLineCol $1), AbsLatte.ECast (Just (tokenLineCol $1)) (snd $3)) 
}
| '-' Expr6 {
  (Just (tokenLineCol $1), AbsLatte.Neg (Just (tokenLineCol $1)) (snd $2)) 
}
| '!' Expr6 {
  (Just (tokenLineCol $1), AbsLatte.Not (Just (tokenLineCol $1)) (snd $2)) 
}
| Expr6 {
  (fst $1, snd $1)
}

Expr4 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr4 MulOp Expr5 {
  (fst $1, AbsLatte.EMul (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr5 {
  (fst $1, snd $1)
}

Expr3 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr3 AddOp Expr4 {
  (fst $1, AbsLatte.EAdd (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr4 {
  (fst $1, snd $1)
}

Expr2 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr2 RelOp Expr3 {
  (fst $1, AbsLatte.ERel (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr3 {
  (fst $1, snd $1)
}

Expr1 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr2 '&&' Expr1 {
  (fst $1, AbsLatte.EAnd (fst $1)(snd $1)(snd $3)) 
}
| Expr2 {
  (fst $1, snd $1)
}

Expr :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr1 '||' Expr {
  (fst $1, AbsLatte.EOr (fst $1)(snd $1)(snd $3)) 
}
| Expr1 {
  (fst $1, snd $1)
}

ListExpr :: {
  (Maybe (Int, Int), [Expr (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| Expr {
  (fst $1, (:[]) (snd $1)) 
}
| Expr ',' ListExpr {
  (fst $1, (:) (snd $1)(snd $3)) 
}

AddOp :: {
  (Maybe (Int, Int), AddOp (Maybe (Int, Int)))
}
: '+' {
  (Just (tokenLineCol $1), AbsLatte.Plus (Just (tokenLineCol $1)))
}
| '-' {
  (Just (tokenLineCol $1), AbsLatte.Minus (Just (tokenLineCol $1)))
}

MulOp :: {
  (Maybe (Int, Int), MulOp (Maybe (Int, Int)))
}
: '*' {
  (Just (tokenLineCol $1), AbsLatte.Times (Just (tokenLineCol $1)))
}
| '/' {
  (Just (tokenLineCol $1), AbsLatte.Div (Just (tokenLineCol $1)))
}
| '%' {
  (Just (tokenLineCol $1), AbsLatte.Mod (Just (tokenLineCol $1)))
}

RelOp :: {
  (Maybe (Int, Int), RelOp (Maybe (Int, Int)))
}
: '<' {
  (Just (tokenLineCol $1), AbsLatte.LTH (Just (tokenLineCol $1)))
}
| '<=' {
  (Just (tokenLineCol $1), AbsLatte.LE (Just (tokenLineCol $1)))
}
| '>' {
  (Just (tokenLineCol $1), AbsLatte.GTH (Just (tokenLineCol $1)))
}
| '>=' {
  (Just (tokenLineCol $1), AbsLatte.GE (Just (tokenLineCol $1)))
}
| '==' {
  (Just (tokenLineCol $1), AbsLatte.EQU (Just (tokenLineCol $1)))
}
| '!=' {
  (Just (tokenLineCol $1), AbsLatte.NE (Just (tokenLineCol $1)))
}

{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pProgram = (>>= return . snd) . pProgram_internal
}

