module State

import Terminal
import Cursor
import Document

export
data EditorMode : Type where
  Normal : Cursor -> EditorMode

export
data State : Type where
  MkState : EditorMode -> Document -> State

export
showState : State -> IO ()
showState (MkState (Normal (MkCursor x y)) (MkDocument lines)) = do
  clearScreen
  traverse_ putStrLn lines
  moveCursor x y

export
initState : Document -> State
initState doc = MkState (Normal (MkCursor 0 0)) doc

