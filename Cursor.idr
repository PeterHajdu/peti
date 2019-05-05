module Cursor

public export
data Cursor : Type where
  MkCursor : Nat -> Nat -> Cursor

export
right : Cursor -> Cursor
right (MkCursor x y) = MkCursor (S x) y

export
left : Cursor -> Cursor
left (MkCursor (S x) y) = MkCursor x y
left (MkCursor Z y) = MkCursor Z y

export
up : Cursor -> Cursor
up (MkCursor x (S y)) = MkCursor x y
up (MkCursor x Z) = MkCursor x Z

export
down : Cursor -> Cursor
down (MkCursor x y) = MkCursor x (S y)

export
lineStart : Cursor -> Cursor
lineStart (MkCursor _ y) = MkCursor Z y
