module State

import Terminal
import Cursor
import Document

public export
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

