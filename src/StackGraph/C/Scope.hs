module StackGraph.C.Scope where

import StackGraph.Handle
import StackGraph.Node (Node)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

newtype Stack = Stack { cells :: StackCell }
  deriving stock Generic
  deriving anyclass GStorable

data StackCell = StackCell
  { head :: Handle Node
  , tail :: Handle StackCell
  } deriving stock Generic
    deriving anyclass GStorable

-- where does Variable go
