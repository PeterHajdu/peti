module Editor

import Data.Fuel
import State
import InsertMode
import NormalMode

%default total

public export
data EditorMode = Insert | Normal

data Command : (ty : Type) -> EditorMode -> (ty -> EditorMode) -> Type where
  GetNormalInput : Command NormalInput Normal (const Normal)
  GetInsertInput : Command InsertInput Insert (const Insert)
  ShowState : State -> Command () Insert (const Insert)
  Save : State -> Command () Insert (const Insert)
  (>>=) : Command a state1 state2_fn ->
          ((res: a) -> Command b (state2_fn res) state3_fn) ->
          Command b state1 state3_fn

export
data RunCommand : EditorMode -> Type where
  Do : Command a state1 state2_fn ->
       ((res: a) -> Inf (RunCommand (state2_fn res))) ->
       RunCommand state1
  Stop : RunCommand Insert

namespace RunCommandDo
  (>>=) : Command a state1 state2_fn ->
          ((res: a) -> Inf (RunCommand (state2_fn res))) ->
          RunCommand state1
  (>>=) = Do

shouldStop : InsertInput -> Bool
shouldStop (MkInsert 'q') = True
shouldStop _ = False

export
editor : State -> RunCommand Insert
editor state = do
  ShowState state
  input <- GetInsertInput
  if shouldStop input
  then do
    Save state
    Stop
  else editor $ handleInsertInput input state

private
runCommand : Command a s1 s2-> IO a
runCommand GetNormalInput = MkNormal <$> getChar
runCommand GetInsertInput = MkInsert <$> getChar
runCommand (ShowState state) = showState state
runCommand (Save state) = saveDocument state
runCommand (cmdl >>= next) = do
  result <- runCommand cmdl
  runCommand (next result)

export
run : Fuel -> RunCommand s -> IO ()
run _ Stop = pure ()
run (More fuel) (Do cmd continuation) = do
  result <- runCommand cmd
  run fuel (continuation result)
run Dry _ = pure ()

