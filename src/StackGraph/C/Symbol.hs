{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
module StackGraph.C.Symbol where

import Foreign.C
import StackGraph.C.Slab (Slab)
import GHC.Generics (Generic)
import Foreign.Storable.Generic
import StackGraph.Handle

-- * Symbols

data {-# CTYPE "stack-graphs.h" "struct sg_symbol" #-} Symbol = Symbol { symbol :: CString, symbol_len :: CSize }
  deriving stock (Eq, Show, Generic)
  deriving anyclass GStorable

type Symbols = Slab Symbol

data Stack = Stack
  { cells :: Handle StackCell
  , length :: CSize
  }

data StackCell = StackCell
  { head :: Scoped
  , scopes :: Stack
  }

data Scoped = Scoped
  { unscopedSymbol :: Handle Symbol
  , stack :: Stack
  }
