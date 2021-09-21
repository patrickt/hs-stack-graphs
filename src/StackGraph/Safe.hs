{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
module StackGraph.Safe where

import StackGraph.C qualified as Man
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
  sg <- Man.new
  fn (StackGraph sg)

addSymbols :: StackGraph s -> [ByteString] -> IO (VS.Vector (Handle (Symbol s)))
addSymbols = coerce Man.addSymbols

getSymbols :: StackGraph s -> IO [Symbol s]
getSymbols = coerce Man.symbols
