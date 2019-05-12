module State

import Terminal
import Cursor
import Document
import Data.Vect

public export
data State : Type where
  MkState : Cursor (S n) -> Document (S n) -> State

export
showState : State -> IO ()
showState (MkState cursor@(MkCursor x y) doc) = do
  clearScreen
  showDocument doc cursor
  moveCursor (S x) 1

export
initState : Document (S n) -> State
initState doc = MkState (MkCursor Z last) doc

export
saveDocument : State -> IO ()
saveDocument (MkState _ doc) = do
  saveDocument doc
  pure ()

