{-# LANGUAGE StrictData #-}
module StackGraph.C.Path.Edge where

import StackGraph.Handle
import Foreign.C.Types
import StackGraph.C.Node (NodeId)
import Foreign

data Edge = Edge
  { sourceNodeId :: NodeId
  , precedence :: Int32
  }

data List = List
  { cells :: Handle ListCell
  , direction :: CInt
  , length :: CSize
  }

data ListCell = ListCell
  { head :: Edge
  , tail :: Handle ListCell
  , reversed :: Handle ListCell
  }
