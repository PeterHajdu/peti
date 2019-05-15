module Parser

import Data.Fuel

public export
data Parser : Type -> Type -> Type where
   Continuation : (i -> Parser i o) -> Parser i o
   Finished : Maybe o -> Parser i o

export
runParser : Monad m => Fuel -> m i -> (Parser i o) -> m (Maybe o)
runParser Dry _ _ = pure Nothing
runParser _ _ (Finished result) = pure result
runParser (More rest) input (Continuation parser) = do
  c <- input
  runParser rest input (parser c)
