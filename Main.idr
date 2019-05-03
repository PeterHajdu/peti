module Main

import Data.Fuel
import State
import Editor

%default total

partial
main : IO ()
main = run forever (editor initialState)

