module State

import Terminal
import Cursor
import Document
import Data.Vect

public export
data State : Type where
  MkState : Cursor n -> Document n -> State

export
showState : State -> IO ()
showState (MkState cursor doc) = do
  clearScreen
  showDocument doc
  let (MkCursor x y) = cursor
  moveCursor (S x) (S (finToNat y))

export
initState : Document (S n) -> State
initState doc = MkState (MkCursor Z last) doc

export
saveDocument : State -> IO ()
saveDocument (MkState _ doc) = do
  saveDocument doc
  pure ()

