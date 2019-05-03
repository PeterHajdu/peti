module State

import Terminal
import Cursor
import Document

export
data State : Type where
  MkState : Cursor -> Document -> State

export
showState : State -> IO ()
showState (MkState (MkCursor x y) (MkDocument lines)) = do
  clearScreen
  traverse_ putStrLn lines
  moveCursor x y

export
initialState : State
initialState = MkState (MkCursor 0 0) (MkDocument ["helo", "bello"])

