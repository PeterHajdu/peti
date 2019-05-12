module NormalMode

import State
import Cursor
import Document
import Data.Fin

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
  NormalTop : NormalInput
  NormalBottom : NormalInput

toZ : Fin n -> Fin n
toZ original {n} = case natToFin Z n of
                     Just k => k
                     Nothing => original

export
updateNormalState : NormalInput -> State -> State
updateNormalState NormalUp state@(MkState cur doc) = MkState (up cur) doc
updateNormalState NormalLeft state@(MkState cur doc) = MkState (left cur) doc
updateNormalState NormalRight state@(MkState cur doc) = MkState (right cur) doc
updateNormalState NormalDown state@(MkState cur doc) = MkState (downWithBound cur) doc
updateNormalState NormalDeleteAt state@(MkState cur doc) = MkState cur (deleteAt doc cur)
updateNormalState NormalTop (MkState (MkCursor x y) doc) = MkState (MkCursor x (toZ y)) doc
updateNormalState NormalBottom (MkState (MkCursor x _) doc) = MkState (MkCursor x last) doc
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
    'g' => Just NormalTop
    'G' => Just NormalBottom
    _ => Nothing

export
getNormalInput : IO (Maybe NormalInput)
getNormalInput = parseChar <$> getChar
