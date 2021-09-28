{-# LANGUAGE StrictData #-}
module StackGraph.C.Partial.Symbol where

import StackGraph.Handle
import Foreign.C
import StackGraph.Variable
import StackGraph.C.Symbol qualified as Symbol
import StackGraph.C.Slab (Slab)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

data Stack = Stack
  { cells :: Handle Cell
  , direction :: CInt
  , variable :: Variable Stack
  } deriving stock Generic
    deriving anyclass GStorable

data Cell = Cell
  { head :: Symbol.Scoped
  , tail :: Handle Cell
  , reversed :: Handle Cell
  }

type Cells = Slab Cell
