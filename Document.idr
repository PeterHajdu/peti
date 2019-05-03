module Document

public export
data Document : Type where
  MkDocument : List String -> Document

