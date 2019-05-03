module Cursor

public export
data Cursor : Type where
  MkCursor : Nat -> Nat -> Cursor
