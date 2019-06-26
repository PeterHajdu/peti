module Document

import Data.Fin
import Data.Vect
import Cursor
import Terminal
import Data.Fuel

%default total

public export
data Document : Type where
  MkDocument : Vect (S n) String -> Cursor (S n) -> (filename: String) -> Document

export
Show Document where
  show (MkDocument lines _ _) = show lines

--todo: fix eq instance
--export
--Eq Document where
--  (MkDocument lines1 _ _) == (MkDocument lines2 _  _) = lines1 == lines2

insertToLine : String -> Nat -> Char -> String
insertToLine line x c = let column = minimum x (length line) --todo: check openbsd segfault
                            linestart = substr 0 column line
                            end = substr column (length line) line
                         in linestart ++ (singleton c) ++ end

export
insert : Document -> Char -> Document
insert doc@(MkDocument lines cur@(MkCursor x y) fn) c =
  let line = Vect.index y lines
      newLine = (insertToLine line x c)
      newLines = replaceAt y newLine lines
   in MkDocument newLines (right cur) fn

splitLineAt : String -> Nat -> (String, String)
splitLineAt line x = (substr 0 x line, substr x 100 line)

safeStrTail : String -> String
safeStrTail str = if 0 == (length str) then "" else substr 1 (length str) str

--todo: extract some duplication if possible
export
deleteBack : Document -> Document
deleteBack doc@(MkDocument lines cur@(MkCursor x y) fn) =
  let line = Vect.index y lines
      (firstPart, secondPart) = splitLineAt line (minus x 1)
      newLine = firstPart ++ (safeStrTail secondPart)
      newLines = replaceAt y newLine lines
   in MkDocument newLines (left cur) fn

export
deleteAt : Document -> Document
deleteAt doc@(MkDocument lines cur@(MkCursor x y) fn) =
  let line = Vect.index y lines
      (firstPart, secondPart) = splitLineAt line x
      newLine = firstPart ++ (safeStrTail secondPart)
      newLines = replaceAt y newLine lines
   in MkDocument newLines cur fn

export
deleteLine : Document -> Document
deleteLine (MkDocument lines cur@(MkCursor x y) fn) =
  let newLines = deleteAt y lines
   in case newLines of
        (_ :: _) => let newY = case strengthen y of
                                 Left _ => last
                                 Right ny => ny
                     in MkDocument newLines (MkCursor x newY) fn
        Nil => MkDocument [""] (MkCursor x FZ) fn

export
cursorDownNewLine : Document -> Document
cursorDownNewLine (MkDocument lines (MkCursor x y) fn) =
    let newLineIndex = FS y
        newLines = insertAt newLineIndex "" lines
     in MkDocument newLines (MkCursor Z newLineIndex) fn

export
newLine : Document -> Document
newLine (MkDocument lines (MkCursor x y) fn) =
    let line = Vect.index y lines
        (firstLine, secondLine) = splitLineAt line x
        withTruncatedLine = replaceAt y firstLine lines
        newLines = insertAt (shift 1 y) secondLine withTruncatedLine
     in MkDocument newLines (MkCursor Z (FS y)) fn

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
showDocument : Document -> IO ()
showDocument (MkDocument lines (MkCursor x y) _)  = do
  rows <- getRows
  let middle = divNatNZ rows 2 SIsNotZ
  printLineByLine (limit rows) (S Z) ((finToInteger y) - (toIntegerNat middle)) lines
  moveCursor (S x) middle

export
saveDocument : Document -> IO ()
saveDocument (MkDocument lines _ fn) = do
  writeFile fn (foldl (\acc, line => acc ++ line ++ "\n") "" lines)
  pure ()

export
cursorUp : Document -> Document
cursorUp (MkDocument lines cursor fn) = MkDocument lines (up cursor) fn

export
cursorLeft : Document -> Document
cursorLeft (MkDocument lines cursor fn) = MkDocument lines (left cursor) fn

export
cursorRight : Document -> Document
cursorRight (MkDocument lines cursor fn) = MkDocument lines (right cursor) fn

export
cursorDownInBounds : Document -> Document
cursorDownInBounds (MkDocument lines cursor fn) = MkDocument lines (downInBounds cursor) fn

--todo: this should be much simpler
toZ : Fin n -> Fin n
toZ original {n} = case natToFin Z n of
                     Just k => k
                     Nothing => original

decWith : Nat -> Fin n -> Fin n
decWith y original {n} = case natToFin ((finToNat original) `minus` y) n of
                       Just k => k
                       Nothing => original

export
cursorTop : Document -> Document
cursorTop (MkDocument lines (MkCursor x y) fn) = MkDocument lines (MkCursor x (toZ y)) fn

page : Nat
page = 20

export
cursorPageUp : Document -> Document
cursorPageUp (MkDocument lines (MkCursor x y) fn) = MkDocument lines (MkCursor x (decWith page y)) fn

export
cursorPageDown : Document -> Document
cursorPageDown (MkDocument lines cur fn) = MkDocument lines (downWithnInBounds page cur) fn

export
cursorBottom : Document -> Document
cursorBottom (MkDocument lines (MkCursor x y) fn) = MkDocument lines (MkCursor x last) fn

export
cursorBeginningOfLine : Document -> Document
cursorBeginningOfLine (MkDocument lines (MkCursor x y) fn) = MkDocument lines (MkCursor Z y) fn

export
cursorEndOfLine : Document -> Document
cursorEndOfLine (MkDocument lines (MkCursor x y) fn) = MkDocument lines (MkCursor (pred $ length $ index y lines) y) fn

endOfWord : Document -> Nat
endOfWord (MkDocument lines (MkCursor oldX y) fn) =
  let line = index y lines
      wordEnds = (elemIndices ' ' $ unpack $ line)
      maybeNextEnd = find (\x => x > (S oldX)) wordEnds
   in pred $ maybe (length line) id maybeNextEnd

export
cursorEndOfWord : Document -> Document
cursorEndOfWord doc@(MkDocument lines (MkCursor oldX y) fn) =
  MkDocument lines (MkCursor (endOfWord doc) y) fn

beginningOfWord : Nat -> Document -> Nat
beginningOfWord from (MkDocument lines (MkCursor _ y) fn) =
  let line = index y lines
      wordEnds = reverse $ elemIndices ' ' $ unpack $ line
      maybeNextStart = S <$> find (\x => x < from) wordEnds
   in maybe Z id maybeNextStart

export
cursorBeginningOfWord : Document -> Document
cursorBeginningOfWord  doc@(MkDocument lines (MkCursor oldX y) fn) =
  MkDocument lines (MkCursor (beginningOfWord (pred oldX) doc) y) fn

beginningOfNextWord : Document -> Nat
beginningOfNextWord (MkDocument lines (MkCursor oldX y) fn) =
  let line = index y lines
      wordEnds = elemIndices ' ' $ unpack $ line
      maybeNextStart = S <$> find (\x => x > oldX) wordEnds
   in maybe (length line) id maybeNextStart

export
cursorBeginningOfNextWord : Document -> Document
cursorBeginningOfNextWord doc@(MkDocument lines (MkCursor oldX y) fn) =
  MkDocument lines (MkCursor (beginningOfNextWord doc) y) fn

deleteFromLine : String -> Nat -> Nat -> String
deleteFromLine line from to =
  let from' = minimum from (length line)
      to'   = minimum to (length line)
      linestart = substr 0 from' line
      end = substr to' (length line) line
   in linestart ++ end

export
deleteUntilNextWord : Document -> Document
deleteUntilNextWord  (MkDocument lines (MkCursor oldX y) fn) =
  let line = index y lines
      wordEnds = elemIndices ' ' $ unpack $ line
      maybeNextStart = S <$> find (\x => x > oldX) wordEnds
      nextStart = maybe (length line) id maybeNextStart
      newLine = deleteFromLine line oldX nextStart
      newLines = replaceAt y newLine lines
   in MkDocument newLines (MkCursor oldX y) fn

export
deleteAWord : Document -> Document
deleteAWord  doc@(MkDocument lines (MkCursor x y) fn) =
  let line     = index y lines
      from     = beginningOfWord x doc
      to       = beginningOfNextWord doc
      newLine  = deleteFromLine line from to
      newLines = replaceAt y newLine lines
   in MkDocument newLines (MkCursor from y) fn
