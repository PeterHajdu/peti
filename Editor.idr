module Editor

import Data.Fuel
import State
import InsertMode
import NormalMode
import EditorMode

%default total

normalModeChange : Maybe NormalInput -> EditorMode
normalModeChange (Just NormalInsert) = Insert
normalModeChange (Just NormalQuit) = Quit
normalModeChange _ = Normal

insertModeChange : InsertInput -> EditorMode
insertModeChange (InsertChar _) = Insert
insertModeChange InsertNewLine = Insert
insertModeChange InsertNormal = Normal
insertModeChange InsertBackspace = Insert

data Command : (ty : Type) -> EditorMode -> (ty -> EditorMode) -> Type where
  GetNormalInput : Command (Maybe NormalInput) Normal (\input => normalModeChange input)
  GetInsertInput : Command InsertInput Insert (\input => insertModeChange input)
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

mutual
  export
  normalEditor : State -> RunCommand Normal
  normalEditor state = do
    ShowState state
    maybeInput <- GetNormalInput
    let newState = maybe state (flip updateNormalState $ state) maybeInput
    case maybeInput of
      Nothing => normalEditor newState
      Just NormalInsert => insertEditor newState
      Just NormalUp => normalEditor newState
      Just NormalLeft => normalEditor newState
      Just NormalRight => normalEditor newState
      Just NormalDown => normalEditor newState
      Just NormalDeleteAt => normalEditor newState
      Just NormalTop => normalEditor newState
      Just NormalBottom => normalEditor newState
      Just NormalBeginningOfLine => normalEditor newState
      Just NormalEndOfLine => normalEditor newState
      Just NormalEndOfWord => normalEditor newState
      Just NormalBeginningOfWord => normalEditor newState
      Just NormalSave => do
        Save state
        normalEditor newState
      Just NormalQuit => Stop

  export
  insertEditor : State -> RunCommand Insert
  insertEditor state = do
    ShowState state
    input <- GetInsertInput
    let newState = updateInsertState input state
    case input of
      (InsertChar c) => insertEditor newState
      InsertNewLine => insertEditor newState
      InsertNormal => normalEditor newState
      InsertBackspace => insertEditor newState

private
runCommand : Command a s1 s2-> IO a
runCommand (Pure a) = pure a
runCommand GetNormalInput = getNormalInput
runCommand GetInsertInput = getInsertInput
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
