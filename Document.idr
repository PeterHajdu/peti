module Document

import Cursor

public export
data Document : Type where
  MkDocument : List String -> Document

export
Show Document where
  show (MkDocument lines) = show lines

export
Eq Document where
  (MkDocument lines1) == (MkDocument lines2) = lines1 == lines2

insertToLine : String -> Nat -> Char -> String
insertToLine line x c = let linestart = substr 0 x line
                            end = substr x (length line) line
                         in foldl (++) "" [linestart, singleton c, end]

export
insert : Document -> Cursor -> Char -> Document
insert doc@(MkDocument lines) (MkCursor x y) c =
  case (inBounds y lines) of
    Yes _ => let line = index y lines
                 beginningLines = take y lines
                 endLines = drop (S y) lines
              in if x <= (length line)
                 then MkDocument $ beginningLines ++ ((insertToLine line x c):: endLines)
                 else doc
    No _ => doc

splitLineAt : String -> Nat -> List String
splitLineAt line x = [substr 0 x line, substr x 100 line]

export
newLine : Document -> Cursor -> Document
newLine doc@(MkDocument lines) (MkCursor x y) =
  case (inBounds y lines) of
    Yes _ => let line = index y lines
                 beginningLines = take y lines
                 endLines = drop (S y) lines
              in if x <= (length line)
                 then MkDocument $ beginningLines ++ (splitLineAt line x) ++ endLines
                 else doc
    No _ => doc
