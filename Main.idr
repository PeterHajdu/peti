module Main

import Data.Fuel
import Data.Vect
import State
import Editor
import Document
import Cursor
import Terminal
import EditorMode

%default total

error : String -> IO ()
error = putStrLn

makeSt : String -> (n : Nat ** Vect n String) -> State
makeSt fn ((S _) ** lines) = initState $ MkDocument lines (MkCursor Z FZ) fn
makeSt fn (Z ** lines) = initState $ MkDocument [""] (MkCursor Z FZ) fn

makeState : String -> String -> State
makeState fileName content =
  let lines = (_ ** fromList $ lines content)
   in makeSt fileName lines

partial
main : IO ()
main = do
  arguments <- getArgs
  let (Just fileName) = index' 1 arguments | error "usage: p <filename>"
  Right fileContent <- readFile fileName | error ("unable to open file: " ++ fileName)
  setRaw
  run forever $ normalEditor $ makeState fileName fileContent
