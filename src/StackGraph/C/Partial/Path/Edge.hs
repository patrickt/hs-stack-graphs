module StackGraph.C.Partial.Path.Edge where

import Data.Int
import Foreign.C.Types (CInt, CSize)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import StackGraph.C.Node (NodeId)
import StackGraph.Handle

data Edge = Edge
  { sourceNodeId :: NodeId,
    precedence :: Int32
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)

data List = List
  { cells :: Handle ListCell,
    direction :: CInt,
    length :: CSize
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)


data ListCell = ListCell
  { head :: Edge,
    tail :: Handle ListCell,
    reversed :: Handle ListCell
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)
