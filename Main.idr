module Main

import Data.Fuel
import State
import Editor
import Document
import Terminal
import EditorMode

%default total

error : String -> IO ()
error = putStrLn

partial
main : IO ()
main = do
  arguments <- getArgs
  let (Just filename) = index' 1 arguments | error "usage: p <filename>"
  Right fileContent <- readFile filename | error ("unable to open file: " ++ filename)
  let initialState = initState $ MkDocument (lines fileContent) filename
  setRaw
  run forever (normalEditor initialState)
