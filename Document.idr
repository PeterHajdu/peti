module Document

import Data.Fin
import Data.Vect
import Cursor
import Terminal
import Data.Fuel

%default total

public export
data Document : Nat -> Type where
  MkDocument : Vect (S n) String -> (filename: String) -> Document (S n)

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
newLine : Document (S n) -> Cursor (S n) -> Document (S (S n))
newLine {n} doc@(MkDocument lines fn) (MkCursor x y) =
    let line = Vect.index y lines
        (firstLine, secondLine) = splitLineAt line x
        withTruncatedLine = replaceAt y firstLine lines
        newLines = insertAt (shift 1 y) secondLine withTruncatedLine
     in MkDocument newLines fn

printLineByLine : Fuel -> Nat -> Integer -> Vect n String -> IO ()
printLineByLine Dry _ _ _ = pure ()
printLineByLine (More fuel) y theoreticalRow lines {n} = do
  moveCursor Z y
  let maybeRow = integerToFin theoreticalRow n
  case maybeRow of
    Nothing => do
      putStr "~"
    Just row => do
      putStr $ index row lines
  printLineByLine fuel (S y) (theoreticalRow + 1) lines

export
showDocument : Nat -> Document n -> Cursor n -> IO ()
showDocument middle (MkDocument lines _) (MkCursor _ y) = printLineByLine (limit 40) (S Z) ((finToInteger y) - (toIntegerNat middle)) lines

export
saveDocument : Document n -> IO ()
saveDocument (MkDocument lines fn) = do
  writeFile fn (foldl (\acc, line => acc ++ line ++ "\n") "" lines)
  pure ()

export
endOfLine : Cursor n -> Document n -> Cursor n
endOfLine (MkCursor _ y) (MkDocument lines _) = MkCursor (pred $ length $ index y lines) y

export
endOfWord : Cursor (S n) -> Document (S n) -> Cursor (S n)
endOfWord old@(MkCursor oldX y) (MkDocument lines _) =
  let line = index y lines
      wordEnds = (elemIndices ' ' $ unpack $ line)
      maybeNextEnd = find (\x => x > (S oldX)) wordEnds
      nextEnd = maybe (length line) id maybeNextEnd
   in MkCursor (pred nextEnd) y

export
beginningOfWord : Cursor (S n) -> Document (S n) -> Cursor (S n)
beginningOfWord old@(MkCursor oldX y) (MkDocument lines _) =
  let line = index y lines
      wordEnds = reverse $ elemIndices ' ' $ unpack $ line
      maybeNextStart = S <$> find (\x => x < (pred oldX)) wordEnds
      nextStart = maybe Z id maybeNextStart
   in MkCursor nextStart y
