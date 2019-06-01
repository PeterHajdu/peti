module State

import Terminal
import Document
import Data.Vect

%default total

public export
data State : Type where
  MkState : Document n -> State

export
showState : State -> IO ()
showState (MkState doc) = do
  clearScreen
  showDocument doc

export
initState : Document (S n) -> State
initState doc = MkState doc

export
saveDocument : State -> IO ()
saveDocument (MkState doc) = do
  saveDocument doc
  pure ()

