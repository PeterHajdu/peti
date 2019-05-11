module NormalMode

import State
import Cursor
import Document

public export
data NormalInput : Type where
  NormalUp : NormalInput
  NormalLeft : NormalInput
  NormalRight : NormalInput
  NormalDown : NormalInput
  NormalInsert : NormalInput
  NormalSave : NormalInput
  NormalQuit : NormalInput
  NormalDeleteAt : NormalInput


export
updateNormalState : NormalInput -> State -> State
updateNormalState NormalUp state@(MkState cur doc) = MkState (up cur) doc
updateNormalState NormalLeft state@(MkState cur doc) = MkState (left cur) doc
updateNormalState NormalRight state@(MkState cur doc) = MkState (right cur) doc
updateNormalState NormalDown state@(MkState cur doc) = MkState (downWithBound cur) doc
updateNormalState NormalDeleteAt state@(MkState cur doc) = MkState cur (deleteAt doc cur)
updateNormalState _ state = state

parseChar : Char -> Maybe NormalInput
parseChar c =
  case c of
    'h' => Just NormalLeft
    'j' => Just NormalDown
    'k' => Just NormalUp
    'l' => Just NormalRight
    'i' => Just NormalInsert
    'w' => Just NormalSave
    'q' => Just NormalQuit
    'x' => Just NormalDeleteAt
    _ => Nothing

export
getNormalInput : IO (Maybe NormalInput)
getNormalInput = parseChar <$> getChar
