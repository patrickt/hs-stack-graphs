{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
module StackGraph.IR (module StackGraph.IR) where

import Data.Int
import StackGraph.Foreign
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Foreign

newtype Handle t = Handle Int32

class HasHandle t where
  bridgeHandle :: Int32 -> Handle t
  bridgeHandle = Handle

instance HasHandle Node
instance HasHandle Symbol
instance HasHandle File
instance HasHandle ScopeStackCell
instance HasHandle SymbolStackCell
instance HasHandle PathEdgeListCell
instance HasHandle PartialScopeStackCell
instance HasHandle PartialSymbolStackCell
instance HasHandle PartialPathEdgeListCell
instance HasHandle PartialPath

convCells :: SymbolStackCells -> IO (Vector SymbolStackCell)
convCells (SymbolStackCells items len) = do
  V.generateM (fromIntegral len) (\idx -> undefined)
