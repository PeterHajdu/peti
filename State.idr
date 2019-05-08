module State

import Input
import Terminal
import Cursor
import Document

export
data State : Type where
  MkState : Cursor -> Document -> State

printLine : (Nat, String) -> IO ()
printLine (i, line) = do
  moveCursor Z i
  putStr line

export
showState : State -> IO ()
showState (MkState cursor (MkDocument lines _)) = do
  clearScreen
  traverse_ printLine (List.zip [1..(length lines)] lines)
  let (MkCursor x y) = cursor
  moveCursor (S x) (S y)

export
initState : Document -> State
initState doc = MkState (MkCursor Z Z) doc

export
saveDocument : State -> IO ()
saveDocument (MkState _ (MkDocument lines fn)) = do
  writeFile fn (unlines lines)
  pure ()

export
handleInput : InsertInput -> State -> State
handleInput (MkInsert c) state@(MkState cur doc) =
  case c of
    'l' => MkState (right cur) doc
    'h' => MkState (left cur) doc
    'k' => MkState (up cur) doc
    'j' => MkState (down cur) doc
    'i' => MkState (cur) doc
    _ => state
--handleInput (MkInsert c) state@(MkState cur doc) =
--  case (ord c) of
--    27 => MkState cur doc
--    13 => let nextDoc = newLine doc cur
--              nextCur = lineStart $ down cur
--           in MkState nextCur nextDoc
--    _ => let nextDoc = insert doc cur c
--             nextCur = right cur
--          in MkState nextCur nextDoc
