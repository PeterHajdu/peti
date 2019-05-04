module State

import Input
import Terminal
import Cursor
import Document

export
data EditorMode : Type where
  Normal : Cursor -> EditorMode
  Input : Cursor -> EditorMode

cursor : EditorMode -> Cursor
cursor (Normal cur) = cur
cursor (Input cur) = cur

export
data State : Type where
  MkState : EditorMode -> Document -> State

printLine : (Nat, String) -> IO ()
printLine (i, line) = do
  moveCursor Z i
  putStr line

export
showState : State -> IO ()
showState (MkState mode (MkDocument lines)) = do
  clearScreen
  traverse_ printLine (List.zip [1..(length lines)] lines)
  let (MkCursor x y) = cursor mode
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
    'i' => MkState (Input $ cur) doc
    _ => state
handleInput (CharInput c) state@(MkState (Input cur) doc) =
  case c of
    '\ESC' => MkState (Normal cur) doc
    _ => let nextDoc = insert doc cur c
             nextCur = right cur
          in MkState (Input nextCur) nextDoc
