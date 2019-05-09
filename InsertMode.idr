module InsertMode

import State
import Cursor
import Document

public export
data InsertInput : Type where
  InsertChar : Char -> InsertInput
  InsertNormal : InsertInput
  InsertNewLine : InsertInput

export
updateInsertState : InsertInput -> State -> State
updateInsertState (InsertChar c) state@(MkState cur doc) = let nextDoc = insert doc cur c
                                                               nextCur = right cur
                                                            in MkState nextCur nextDoc
updateInsertState InsertNewLine state@(MkState cur doc) = let nextDoc = newLine doc cur
                                                              nextCur = lineStart $ down cur
                                                           in MkState nextCur nextDoc
updateInsertState InsertNormal state = state

export
getInsertInput : IO InsertInput
getInsertInput = InsertChar <$> getChar

parseChar : Char -> InsertInput
parseChar c =
  case (ord c) of
    27 => InsertNormal
    13 => InsertNewLine
    _ => InsertChar c
