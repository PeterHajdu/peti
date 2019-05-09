module NormalMode

import State
import Cursor
import Document

public export
data NormalInput : Type where
  MkNormal : Char -> NormalInput

export
handleNormalInput : NormalInput -> State -> State
handleNormalInput (MkNormal c) state@(MkState cur doc) =
  case c of
    'l' => MkState (right cur) doc
    'h' => MkState (left cur) doc
    'k' => MkState (up cur) doc
    'j' => MkState (down cur) doc
    'i' => MkState (cur) doc
    _ => state
