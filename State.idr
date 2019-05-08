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
showState (MkState mode (MkDocument lines _)) = do
  clearScreen
  traverse_ printLine (List.zip [1..(length lines)] lines)
  let (MkCursor x y) = cursor mode
  moveCursor (S x) (S y)

export
initState : Document -> State
initState doc = MkState (Normal (MkCursor Z Z)) doc

export
saveDocument : State -> IO ()
saveDocument (MkState _ (MkDocument lines fn)) = do
  writeFile fn (unlines lines)
  pure ()

export
handleInput : InsertInput -> State -> State
handleInput (MkInsert c) state@(MkState (Normal cur) doc) =
  case c of
    'l' => MkState (Normal $ right cur) doc
    'h' => MkState (Normal $ left cur) doc
    'k' => MkState (Normal $ up cur) doc
    'j' => MkState (Normal $ down cur) doc
    'i' => MkState (Input $ cur) doc
    _ => state
handleInput (MkInsert c) state@(MkState (Input cur) doc) =
  case (ord c) of
    27 => MkState (Normal cur) doc
    13 => let nextDoc = newLine doc cur
              nextCur = lineStart $ down cur
           in MkState (Input nextCur) nextDoc
    _ => let nextDoc = insert doc cur c
             nextCur = right cur
          in MkState (Input nextCur) nextDoc
