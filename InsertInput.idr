module InsertInput

import State
import Cursor
import Document

%default total

public export
data InsertInput : Type where
  InsertChar : Char -> InsertInput
  InsertNormal : InsertInput
  InsertNewLine : InsertInput
  InsertBackspace : InsertInput

parseChar : Char -> InsertInput
parseChar c =
  case (ord c) of
    127 => InsertBackspace
    13 => InsertNewLine
    27 => InsertNormal
    _ => InsertChar c

export
getInsertInput : IO InsertInput
getInsertInput = parseChar <$> getChar
