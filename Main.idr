module Main

import Data.Fuel
import Data.Vect
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
  let initialState = initState $ MkDocument (""::(fromList $ lines fileContent)) filename --todo: solve initstate (S n) issue
  setRaw
  run forever (normalEditor initialState)
