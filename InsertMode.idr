module InsertMode

import State
import Cursor
import Document

public export
data InsertInput : Type where
  InsertChar : Char -> InsertInput
  InsertNormal : InsertInput
  InsertNewLine : InsertInput
  InsertBackspace : InsertInput

export
updateInsertState : InsertInput -> State -> State
updateInsertState (InsertChar c) state@(MkState cur doc) = let nextDoc = insert doc cur c
                                                               nextCur = right cur
                                                            in MkState nextCur nextDoc
updateInsertState InsertNewLine state@(MkState cur doc) = let nextDoc = newLine doc cur
                                                              nextCur = lineStart $ down cur
                                                           in MkState nextCur nextDoc
updateInsertState InsertBackspace state@(MkState cur doc) = let nextDoc = deleteBack doc cur
                                                             in MkState (left cur) nextDoc
updateInsertState InsertNormal state = state


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
