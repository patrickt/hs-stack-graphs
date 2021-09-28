{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module StackGraph.C.File where

import Foreign.C
import Foreign.Storable.Generic (GStorable)
import StackGraph.C.Slab (Slab)
import GHC.Generics (Generic)

data {-# CTYPE "stack-graphs.h" "struct sg_file" #-} File = File { name :: CString, name_len :: CSize }
  deriving stock (Generic)
  deriving anyclass GStorable

type Files = Slab File
