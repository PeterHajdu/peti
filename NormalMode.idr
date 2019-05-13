module NormalMode

import Data.Fuel
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
  NormalBeginningOfLine : NormalInput

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
updateNormalState _ state = state

data InputParser : Type where
   Continuation : (Char -> InputParser) -> InputParser
   Finished : Maybe NormalInput -> InputParser

parser : InputParser
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
    _ => Finished Nothing

parseInput : Fuel -> InputParser -> IO (Maybe NormalInput)
parseInput Dry _ = pure Nothing
parseInput _ (Finished result) = pure result
parseInput (More rest) (Continuation parser) = do
  c <- getChar
  parseInput rest (parser c)

export
getNormalInput : IO (Maybe NormalInput)
getNormalInput = parseInput (limit 3) parser
