module NormalMode

import Data.Fuel
import State
import Cursor
import Document
import Data.Fin
import Parser

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
  NormalBeginningOfLine : NormalInput
  NormalEndOfLine : NormalInput
  NormalEndOfWord : NormalInput

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
updateNormalState NormalBeginningOfLine (MkState (MkCursor _ y) doc) = MkState (MkCursor Z y) doc
updateNormalState NormalEndOfLine (MkState cur doc) = MkState (endOfLine cur doc) doc
updateNormalState NormalEndOfWord (MkState cur doc) = MkState (endOfWord cur doc) doc
updateNormalState _ state = state

parser : Parser Char NormalInput
parser = Continuation $ \c1 => case c1 of
    'h' => Finished $ Just NormalLeft
    'j' => Finished $ Just NormalDown
    'k' => Finished $ Just NormalUp
    'l' => Finished $ Just NormalRight
    'i' => Finished $ Just NormalInsert
    'w' => Finished $ Just NormalSave
    'q' => Finished $ Just NormalQuit
    'x' => Finished $ Just NormalDeleteAt
    'G' => Finished $ Just NormalBottom
    'g' => Continuation $ \c2 => Finished $ if c2 == 'g' then Just NormalTop else Nothing
    '|' => Finished $ Just NormalBeginningOfLine
    'e' => Finished $ Just NormalEndOfWord
    _ => Finished Nothing

export
getNormalInput : IO (Maybe NormalInput)
getNormalInput = runParser (limit 3) getChar parser
