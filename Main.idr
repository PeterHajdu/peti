module Main

import Data.Fuel
import State
import Editor
import Document
import Terminal

%default total

partial
main : IO ()
main = do
  arguments <- getArgs
  let (Just filename) = index' 1 arguments
  Right fileContent <- readFile filename
  let initialState = initState $ MkDocument $ lines fileContent
  setRaw
  run forever (editor initialState)

