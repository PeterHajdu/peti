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
  NormalBeginningOfNextWord : NormalInput
  NormalDeleteUntilNextWord : NormalInput
  NormalDeleteAWord : NormalInput
  NormalChangeAWord : NormalInput
  NormalChangeUntilNextWord : NormalInput
  NormalBeginningOfWord : NormalInput
  NormalPageUp : NormalInput
  NormalPageDown : NormalInput
  NormalInsertNewLine : NormalInput
  NormalInsertRight : NormalInput
  NormalInsertBeginningOfLine : NormalInput
  NormalInsertEndOfLine : NormalInput
  NormalInsertNewLineUp : NormalInput

deleteParser : Parser Char NormalInput
deleteParser =
  Continuation $ \c2 => case c2 of
                          'a' => Continuation $ \c3 => Finished $ if c3 == 'w' then Just NormalDeleteAWord else Nothing
                          'd' => Finished $ Just NormalDeleteLine
                          'w' => Finished $ Just NormalDeleteUntilNextWord
                          _   => Finished Nothing

changeParser : Parser Char NormalInput
changeParser =
  Continuation $ \c2 => case c2 of
                          'a' => Continuation $ \c3 => Finished $ if c3 == 'w' then Just NormalChangeAWord else Nothing
                          'w' => Finished $ Just NormalChangeUntilNextWord
                          _ => Finished Nothing

parser : Parser Char NormalInput
parser = Continuation $ \c1 => case (ord c1) of
    104 => Finished $ Just NormalLeft
    106 => Finished $ Just NormalDown
    107 => Finished $ Just NormalUp
    108 => Finished $ Just NormalRight
    105 => Finished $ Just NormalInsert
    111 => Finished $ Just NormalInsertNewLine
    79 => Finished $ Just NormalInsertNewLineUp
    97 => Finished $ Just NormalInsertRight
    73 => Finished $ Just NormalInsertBeginningOfLine
    65 => Finished $ Just NormalInsertEndOfLine
    115 => Finished $ Just NormalSave
    113 => Finished $ Just NormalQuit
    120 => Finished $ Just NormalDeleteAt
    71 => Finished $ Just NormalBottom
    103 => Continuation $ \c2 => Finished $ if c2 == 'g' then Just NormalTop else Nothing
    100 => deleteParser
    124 => Finished $ Just NormalBeginningOfLine
    36 => Finished $ Just NormalEndOfLine
    101 => Finished $ Just NormalEndOfWord
    119 => Finished $ Just NormalBeginningOfNextWord
    98 => Finished $ Just NormalBeginningOfWord
    21 => Finished $ Just NormalPageUp
    4 => Finished $ Just NormalPageDown
    99 => changeParser
    _ => Finished Nothing

export
getNormalInput : IO (Maybe NormalInput)
getNormalInput = runParser (limit 4) getChar parser
