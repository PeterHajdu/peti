module Editor

import Data.Fuel
import State
import Input

%default total

public export
data EditorState = Insert | Normal

data Command : (ty : Type) -> EditorState -> (ty -> EditorState) -> Type where
  GetInput : Command Input Insert (const Insert)
  GetNormatInput : Command NormalInput Normal (const Normal)
  GetInsertInput : Command InsertInput Insert (const Insert)
  ShowState : State -> Command () Insert (const Insert)
  Save : State -> Command () Insert (const Insert)
  (>>=) : Command a state1 state2_fn ->
          ((res: a) -> Command b (state2_fn res) state3_fn) ->
          Command b state1 state3_fn

export
data RunCommand : EditorState -> Type where
  Do : Command a state1 state2_fn ->
       ((res: a) -> Inf (RunCommand (state2_fn res))) ->
       RunCommand state1
  Stop : RunCommand Insert

namespace RunCommandDo
  (>>=) : Command a state1 state2_fn ->
          ((res: a) -> Inf (RunCommand (state2_fn res))) ->
          RunCommand state1
  (>>=) = Do

shouldStop : Input -> Bool
shouldStop (CharInput 'q') = True
shouldStop _ = False

export
editor : State -> RunCommand Insert
editor state = do
  ShowState state
  input <- GetInput
  if shouldStop input
  then do
    Save state
    Stop
  else editor $ handleInput input state

private
runCommand : Command a s1 s2-> IO a
runCommand GetInput = CharInput <$> getChar
runCommand GetNormatInput = MkNormal <$> getChar
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

