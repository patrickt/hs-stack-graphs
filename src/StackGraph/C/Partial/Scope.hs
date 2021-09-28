{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}
module StackGraph.C.Partial.Scope where

import StackGraph.Handle
import Foreign.C.Types
import StackGraph.C.Path.Edge.List qualified as Path.Edge.List
import StackGraph.C.Node (Node)
import StackGraph.Variable
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

data Stack = Stack
  { cells :: Handle StackCell
  , direction :: CInt
  , variable :: Variable Stack
  } deriving stock Generic
    deriving anyclass GStorable

data StackCell = StackCell
  { head :: Handle Node
  , tail :: Handle Path.Edge.List.Cell
  , reversed :: Handle Path.Edge.List.Cell
  }
