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
  NormalPageUp : NormalInput
  NormalPageDown : NormalInput
  NormalInsertNewLine : NormalInput

parser : Parser Char NormalInput
parser = Continuation $ \c1 => case (ord c1) of
    104 => Finished $ Just NormalLeft
    106 => Finished $ Just NormalDown
    107 => Finished $ Just NormalUp
    108 => Finished $ Just NormalRight
    105 => Finished $ Just NormalInsert
    111 => Finished $ Just NormalInsertNewLine
    119 => Finished $ Just NormalSave
    113 => Finished $ Just NormalQuit
    120 => Finished $ Just NormalDeleteAt
    71 => Finished $ Just NormalBottom
    103 => Continuation $ \c2 => Finished $ if c2 == 'g' then Just NormalTop else Nothing
    100 => Continuation $ \c2 => Finished $ if c2 == 'd' then Just NormalDeleteLine else Nothing
    124 => Finished $ Just NormalBeginningOfLine
    36 => Finished $ Just NormalEndOfLine
    101 => Finished $ Just NormalEndOfWord
    98 => Finished $ Just NormalBeginningOfWord
    21 => Finished $ Just NormalPageUp
    4 => Finished $ Just NormalPageDown
    _ => Finished Nothing

export
getNormalInput : IO (Maybe NormalInput)
getNormalInput = runParser (limit 3) getChar parser
