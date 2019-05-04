module State

import Input
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
  moveCursor (S x) (S y)

export
initState : Document -> State
initState doc = MkState (Normal (MkCursor Z Z)) doc

export
handleInput : Input -> State -> State
handleInput (CharInput c) state@(MkState (Normal cur) doc) =
  case c of
    'l' => MkState (Normal $ right cur) doc
    'h' => MkState (Normal $ left cur) doc
    'k' => MkState (Normal $ up cur) doc
    'j' => MkState (Normal $ down cur) doc
    _ => state
