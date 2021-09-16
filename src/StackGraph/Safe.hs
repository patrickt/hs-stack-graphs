{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
module StackGraph.Safe where

import StackGraph.Manual qualified as Man
import Data.Word
import Data.ByteString (ByteString)
import Data.Coerce
import StackGraph.Handle
import Data.Vector.Storable qualified as VS
import Data.Vector qualified as V

newtype StackGraph s = StackGraph Man.StackGraph

newtype Symbol s = Symbol ByteString

withStackGraph :: (forall s . StackGraph s -> IO a) -> IO a
withStackGraph fn = do
  sg <- Man.stackGraphNew
  fn (StackGraph sg)

addSymbols :: StackGraph s -> [ByteString] -> IO (VS.Vector (Handle (Symbol s)))
addSymbols = coerce Man.stackGraphAddSymbols

getSymbols :: StackGraph s -> IO (V.Vector (Symbol s))
getSymbols = coerce Man.stackGraphSymbols
