module State

import Terminal
import Cursor
import Document
import Data.Vect

public export
data State : Type where
  MkState : Cursor -> Document n -> State

printLine : (Nat, String) -> IO ()
printLine (i, line) = do
  moveCursor Z i
  putStr line

export
showState : State -> IO ()
showState (MkState cursor (MkDocument lines _)) = do
  clearScreen
  --traverse_ printLine (Data.Vect.zip [1..(length lines)] lines)
  let (MkCursor x y) = cursor
  moveCursor (S x) (S y)

export
initState : Document n -> State
initState doc = MkState (MkCursor Z Z) doc

export
saveDocument : State -> IO ()
saveDocument (MkState _ (MkDocument lines fn)) = do
  writeFile fn (foldl (++) "" lines)
  pure ()

