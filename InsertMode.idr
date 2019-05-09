module InsertMode

import State
import Cursor
import Document

public export
data InsertInput : Type where
  MkInsert : Char -> InsertInput

export
handleInsertInput : InsertInput -> State -> State
handleInsertInput (MkInsert c) state@(MkState cur doc) =
  case (ord c) of
    27 => MkState cur doc
    13 => let nextDoc = newLine doc cur
              nextCur = lineStart $ down cur
           in MkState nextCur nextDoc
    _ => let nextDoc = insert doc cur c
             nextCur = right cur
          in MkState nextCur nextDoc
