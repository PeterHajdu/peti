module NormalInput

import Data.Fuel
import State
import Cursor
import Document
import Data.Fin
import Parser

%default total

public export
data NormalInput : Type where
  NormalUp : NormalInput
  NormalLeft : NormalInput
  NormalRight : NormalInput
  NormalDown : NormalInput
  NormalInsert : NormalInput
  NormalSave : NormalInput
  NormalQuit : NormalInput
  NormalDeleteAt : NormalInput
  NormalDeleteLine : NormalInput
  NormalTop : NormalInput
  NormalBottom : NormalInput
  NormalBeginningOfLine : NormalInput
  NormalEndOfLine : NormalInput
  NormalEndOfWord : NormalInput
  NormalBeginningOfWord : NormalInput

parser : Parser Char NormalInput
parser = Continuation $ \c1 => case c1 of
    'h' => Finished $ Just NormalLeft
    'j' => Finished $ Just NormalDown
    'k' => Finished $ Just NormalUp
    'l' => Finished $ Just NormalRight
    'i' => Finished $ Just NormalInsert
    'w' => Finished $ Just NormalSave
    'q' => Finished $ Just NormalQuit
    'x' => Finished $ Just NormalDeleteAt
    'G' => Finished $ Just NormalBottom
    'g' => Continuation $ \c2 => Finished $ if c2 == 'g' then Just NormalTop else Nothing
    'd' => Continuation $ \c2 => Finished $ if c2 == 'd' then Just NormalDeleteLine else Nothing
    '|' => Finished $ Just NormalBeginningOfLine
    '$' => Finished $ Just NormalEndOfLine
    'e' => Finished $ Just NormalEndOfWord
    'b' => Finished $ Just NormalBeginningOfWord
    _ => Finished Nothing

export
getNormalInput : IO (Maybe NormalInput)
getNormalInput = runParser (limit 3) getChar parser
