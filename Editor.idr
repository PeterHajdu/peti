module Editor

import Data.Fuel
import State
import InsertInput
import NormalInput
import EditorMode
import Document
import Cursor
import Data.Fin

%default total

normalModeChange : Maybe NormalInput -> EditorMode
normalModeChange (Just NormalInsert) = Insert
normalModeChange (Just NormalInsertNewLine) = Insert
normalModeChange (Just NormalInsertNewLineUp) = Insert
normalModeChange (Just NormalInsertRight) = Insert
normalModeChange (Just NormalInsertBeginningOfLine) = Insert
normalModeChange (Just NormalInsertEndOfLine) = Insert
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
    let (MkState doc) = state
    case maybeInput of
      Nothing => normalEditor state
      Just NormalInsert => insertEditor state
      Just NormalInsertNewLine => insertEditor $ MkState $ cursorDownNewLine doc
      Just NormalInsertNewLineUp => insertEditor $ MkState $ cursorUp $ newLine $ cursorBeginningOfLine $ doc --todo: this should be simpler
      Just NormalInsertRight => insertEditor $ MkState $ cursorRight doc
      Just NormalInsertBeginningOfLine => insertEditor $ MkState $ cursorBeginningOfLine doc
      Just NormalInsertEndOfLine => insertEditor $ MkState $ cursorRight $ cursorEndOfLine doc
      Just NormalUp => normalEditor $ MkState $ cursorUp doc
      Just NormalLeft => normalEditor $ MkState $ cursorLeft doc
      Just NormalRight => normalEditor $ MkState $ cursorRight doc
      Just NormalDown => normalEditor $ MkState $ cursorDownInBounds doc
      Just NormalDeleteAt => normalEditor $ MkState (deleteAt doc)
      Just NormalDeleteLine => normalEditor $ MkState (deleteLine doc)
      Just NormalTop => normalEditor $ MkState $ cursorTop doc
      Just NormalBottom => normalEditor $ MkState $ cursorBottom doc
      Just NormalBeginningOfLine => normalEditor $ MkState $ cursorBeginningOfLine doc
      Just NormalEndOfLine => normalEditor $ MkState $ cursorEndOfLine doc
      Just NormalEndOfWord => normalEditor $ MkState $ cursorEndOfWord doc
      Just NormalBeginningOfNextWord => normalEditor $ MkState $ cursorBeginningOfNextWord doc
      Just NormalBeginningOfWord => normalEditor $ MkState $ cursorBeginningOfWord doc
      Just NormalPageUp => normalEditor $ MkState $ cursorPageUp doc
      Just NormalPageDown => normalEditor $ MkState $ cursorPageDown doc
      Just NormalSave => do
        Save state
        normalEditor state
      Just NormalQuit => Stop

  export
  insertEditor : State -> RunCommand Insert
  insertEditor state = do
    ShowState state
    input <- GetInsertInput
    let (MkState doc) = state
    case input of
      (InsertChar c) => insertEditor $ MkState (insert doc c)
      InsertNewLine => insertEditor $ MkState (newLine doc)
      InsertNormal => normalEditor state
      InsertBackspace => insertEditor $ MkState (deleteBack doc)

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
