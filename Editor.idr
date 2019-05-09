module Editor

import Data.Fuel
import State
import InsertMode
import NormalMode
import EditorMode

%default total

data Command : (ty : Type) -> EditorMode -> (ty -> EditorMode) -> Type where
  GetNormalInput : Command (Maybe NormalInput) Normal (const Normal)
  GetInsertInput : Command InsertInput Insert (const Insert)
  ShowState : State -> Command () s (const s)
  Save : State -> Command () Normal (const Normal)
  Pure : a -> Command a s (const s)
  (>>=) : Command a state1 state2_fn ->
          ((res: a) -> Command b (state2_fn res) state3_fn) ->
          Command b state1 state3_fn

export
data RunCommand : EditorMode -> Type where
  Do : Command a state1 state2_fn ->
       ((res: a) -> Inf (RunCommand (state2_fn res))) ->
       RunCommand state1
  Stop : RunCommand Quit

namespace RunCommandDo
  (>>=) : Command a state1 state2_fn ->
          ((res: a) -> Inf (RunCommand (state2_fn res))) ->
          RunCommand state1
  (>>=) = Do

normalModeChange : input -> EditorMode
normalModeChange _ = Normal

handleNormalInput : State -> (input: NormalInput) -> Command () Normal (const $ normalModeChange input)
handleNormalInput state NormalSave = Save state
handleNormalInput state _ = Pure ()

export
editor : State -> RunCommand Normal
editor state = do
  ShowState state
  maybeInput <- GetNormalInput
  let newState = maybe state (flip updateNormalState $ state) maybeInput
  let command = maybe (Pure ()) (handleNormalInput state) maybeInput
  editor newState

private
runCommand : Command a s1 s2-> IO a
runCommand (Pure a) = pure a
runCommand GetNormalInput = getNormalInput
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

