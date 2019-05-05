module Document

import Cursor

public export
data Document : Type where
  MkDocument : List String -> (filename: String) -> Document

export
Show Document where
  show (MkDocument lines _) = show lines

export
Eq Document where
  (MkDocument lines1 _) == (MkDocument lines2 _) = lines1 == lines2

insertToLine : String -> Nat -> Char -> String
insertToLine line x c = let linestart = substr 0 x line
                            end = substr x (length line) line
                         in foldl (++) "" [linestart, singleton c, end]

export
insert : Document -> Cursor -> Char -> Document
insert doc@(MkDocument lines fn) (MkCursor x y) c =
  case (inBounds y lines) of
    Yes _ => let line = index y lines
                 beginningLines = take y lines
                 endLines = drop (S y) lines
              in if x <= (length line)
                 then MkDocument (beginningLines ++ ((insertToLine line x c):: endLines)) fn
                 else doc
    No _ => doc

splitLineAt : String -> Nat -> List String
splitLineAt line x = [substr 0 x line, substr x 100 line]

export
newLine : Document -> Cursor -> Document
newLine doc@(MkDocument lines fn) (MkCursor x y) =
  case (inBounds y lines) of
    Yes _ => let line = index y lines
                 beginningLines = take y lines
                 endLines = drop (S y) lines
              in if x <= (length line)
                 then MkDocument (beginningLines ++ (splitLineAt line x) ++ endLines) fn
                 else doc
    No _ => doc
