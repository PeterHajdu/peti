module Main

import Terminal


partial
loop : IO ()
loop = do
  c <- getChar
  putStrLn $ show (ord c)
  if c=='q' then pure () else loop

partial
main : IO ()
main = do
  setRaw
  loop
