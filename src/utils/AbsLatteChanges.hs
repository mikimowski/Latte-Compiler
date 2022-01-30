instance Show Ident where
  show (Ident ident) = ident

instance Show (Type a) where
    show (TArray _ t) = show t ++ "[]"
    show (TClass _ ident) = show ident
    show (TInt _) = "int"
    show (TStr _) = "string"
    show (TBool _) = "boolean"
    show (TVoid _) = "void"


instance (Eq a) => Eq (Type a) where
  (TClass _ ident1) == (TClass _ ident2) = ident1 == ident2
  (TInt _) == (TInt _) = True
  (TStr _) == (TStr _) = True
  (TBool _) == (TBool _) = True
  (TVoid _) == (TVoid _) = True
  (TArray _ type1) == (TArray _ type2) = type1 == type2
  _ == _ = False