{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module StackGraph.C.Symbol where

import Foreign.C
import StackGraph.C.Arena (Arena)
import GHC.Generics (Generic)
import Foreign.Storable.Generic

-- * Symbols

data {-# CTYPE "stack-graphs.h" "struct sg_symbol" #-} Symbol = Symbol { symbol :: CString, symbol_len :: CSize }
  deriving stock (Eq, Show, Generic)
  deriving anyclass GStorable

type Symbols = Arena Symbol
