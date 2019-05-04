module Editor

import Data.Fuel
import State
import Input

%default total

data Command : Type -> Type where
  GetInput : Command Input
  ShowState : State -> Command ()

export
data RunCommand : Type where
  Do : Command a -> (a -> Inf RunCommand) -> RunCommand
  Stop : RunCommand

(>>=) : Command a -> (a -> Inf RunCommand) -> RunCommand
(>>=) = Do

shouldStop : Input -> Bool
shouldStop (CharInput 'q') = True
shouldStop _ = False

export
editor : State -> RunCommand
editor state = do
  ShowState state
  input <- GetInput
  if shouldStop input
  then Stop
  else editor $ handleInput input state

private
runCommand : Command a -> IO a
runCommand GetInput = CharInput <$> getChar
runCommand (ShowState state) = showState state

export
run : Fuel -> RunCommand -> IO ()
run _ Stop = pure ()
run (More fuel) (Do cmd continuation) = do
  result <- runCommand cmd
  run fuel (continuation result)
run Dry _ = pure ()

