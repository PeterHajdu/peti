module Cursor

import Data.Fin

%default total

public export
data Cursor : Nat -> Type where
  MkCursor : Nat -> Fin n -> Cursor n

export
right : Cursor n -> Cursor n
right (MkCursor x y) = MkCursor (S x) y

export
left : Cursor n -> Cursor n
left (MkCursor (S x) y) = MkCursor x y
left (MkCursor Z y) = MkCursor Z y

dec : Fin n -> Fin n
dec FZ = FZ
dec (FS k) = weaken k

export
up : Cursor n -> Cursor n
up (MkCursor x y) = MkCursor x (dec y)

export
down : Cursor n -> Cursor (S n)
down (MkCursor x y) = MkCursor x (shift 1 y)

export
downInBounds : Cursor n -> Cursor n
downInBounds originalCursor@(MkCursor x y) =
  case strengthen $ shift 1 y of
    Left _ => originalCursor
    Right newY => MkCursor x newY

--todo: is it possible to use something like strengthenN and shift?
export
downWithnInBounds : Nat -> Cursor n -> Cursor n
downWithnInBounds d originalCursor@(MkCursor x y) {n} =
  case natToFin ((finToNat y) + d) n of
    Nothing => originalCursor
    Just newY => MkCursor x newY

export
lineStart : Cursor n -> Cursor n
lineStart (MkCursor _ y) = MkCursor Z y
