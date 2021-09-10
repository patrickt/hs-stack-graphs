{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
module StackGraph.Safe where

import StackGraph.Manual qualified as Man
import Data.Word
import Data.ByteString (ByteString)
import Data.Coerce

newtype StackGraph s = StackGraph Man.StackGraph

newtype Symbol s = Symbol ByteString

newtype Handle t = Handle Word32

withStackGraph :: (forall a . StackGraph s -> IO a) -> IO a
withStackGraph fn = do
  sg <- Man.stackGraphNew
  fn (StackGraph sg)

addSymbols :: StackGraph s -> [ByteString] -> IO [Handle (Symbol s)]
addSymbols = coerce Man.stackGraphAddSymbols

getSymbols :: StackGraph s -> IO [Symbol s]
getSymbols = coerce Man.stackGraphSymbols
