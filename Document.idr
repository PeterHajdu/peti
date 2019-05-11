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
insertToLine line x c = let column = minimum x (length line) --todo: check openbsd segfault
                            linestart = substr 0 column line
                            end = substr column (length line) line
                         in linestart ++ (singleton c) ++ end

export
insert : Document n -> Cursor n -> Char -> Document n
insert doc@(MkDocument lines fn) (MkCursor x y) c =
  let line = Vect.index y lines
      newLine = (insertToLine line x c)
      newLines = replaceAt y newLine lines
   in MkDocument newLines fn

splitLineAt : String -> Nat -> (String, String)
splitLineAt line x = (substr 0 x line, substr x 100 line)

safeStrTail : String -> String
safeStrTail str = if 0 == (length str) then "" else substr 1 (length str) str

--todo: extract some duplication if possible
export
deleteBack : Document n -> Cursor n -> Document n
deleteBack doc@(MkDocument lines fn) (MkCursor x y) =
  let line = Vect.index y lines
      (firstPart, secondPart) = splitLineAt line (minus x 1)
      newLine = firstPart ++ (safeStrTail secondPart)
      newLines = replaceAt y newLine lines
   in MkDocument newLines fn

export
deleteAt : Document n -> Cursor n -> Document n
deleteAt doc@(MkDocument lines fn) (MkCursor x y) =
  let line = Vect.index y lines
      (firstPart, secondPart) = splitLineAt line x
      newLine = firstPart ++ (safeStrTail secondPart)
      newLines = replaceAt y newLine lines
   in MkDocument newLines fn

export
newLine : Document n -> Cursor n -> Document (S n)
newLine {n} doc@(MkDocument lines fn) (MkCursor x y) =
    let line = Vect.index y lines
        (firstLine, secondLine) = splitLineAt line x
        withTruncatedLine = replaceAt y firstLine lines
        newLines = insertAt (shift 1 y) secondLine withTruncatedLine
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
  writeFile fn (foldl (\acc, line => acc ++ line ++ "\n") "" lines)
  pure ()
