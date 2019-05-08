module Input

public export
data Input : Type where
  CharInput : Char -> Input

public export
data NormalInput : Type where
  MkNormal : Char -> NormalInput

public export
data InsertInput : Type where
  MkInsert : Char -> InsertInput
