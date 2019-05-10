module Document

import Data.Fin
import Data.Vect
import Cursor
import Terminal

public export
data Document : Nat -> Type where
  MkDocument : Vect n String -> (filename: String) -> Document n

export
Show (Document n) where
  show (MkDocument lines _) = show lines

export
Eq (Document n) where
  (MkDocument lines1 _) == (MkDocument lines2 _) = lines1 == lines2

insertToLine : String -> Nat -> Char -> String
insertToLine line x c = let linestart = substr 0 x line
                            end = substr x (length line) line
                         --in foldl (++) "" [linestart, singleton c, end]
                         in "a"

export
insert : Document n -> Cursor -> Char -> Document n
insert doc@(MkDocument lines fn) (MkCursor x y) c = doc
--  case (inBounds y lines) of
--    Yes _ => let line = index y lines
--                 beginningLines = take y lines
--                 endLines = drop (S y) lines
--              in if x <= (length line)
--                 then MkDocument (beginningLines ++ ((insertToLine line x c):: endLines)) fn
--                 else doc
--    No _ => doc

splitLineAt : String -> Nat -> List String
splitLineAt line x = [substr 0 x line, substr x 100 line]

export
newLine : Document n -> Cursor -> Document (S n)
newLine {n} doc@(MkDocument lines fn) (MkCursor x y) =
  let row = maybe last weaken (natToFin y n)
      newLines = insertAt row "" lines
   in MkDocument newLines fn

printLineByLine : Vect n String -> Nat -> IO ()
printLineByLine Nil _ = pure ()
printLineByLine (line::rest) row = do
  moveCursor Z row
  putStr line
  printLineByLine rest (S row)

export
showDocument : Document n -> IO ()
showDocument (MkDocument lines _) = printLineByLine lines (S Z)

export
saveDocument : Document n -> IO ()
saveDocument (MkDocument lines fn) = do
  writeFile fn (foldl (++) "" lines)
  pure ()
