module StackGraph.C.Path.Edge.List where

import StackGraph.C.Path.Edge (Edge)
import StackGraph.Handle
import StackGraph.C.Slab
import Foreign.C.Types

data List = List
  { cells :: Handle Cell
  , direction :: CInt
  , length :: CSize
  }

data Cell = Cell
  { head :: Edge
  , tail :: Handle Cell
  , reversed :: Handle Cell
  }

type Cells = Slab Cell
