module Document

import Data.Vect
import Cursor

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
newLine : Document n -> Cursor -> Document n
newLine doc@(MkDocument lines fn) (MkCursor x y) = doc
--  case (inBounds y lines) of
--    Yes _ => let line = index y lines
--                 beginningLines = take y lines
--                 endLines = drop (S y) lines
--              in if x <= (length line)
--                 then MkDocument (beginningLines ++ (splitLineAt line x) ++ endLines) fn
--                 else doc
--    No _ => doc
