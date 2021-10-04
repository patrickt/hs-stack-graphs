-- | A 'Handle' over some type @t@ represents a reference to a @t@ managed in some
-- other block of memory (usually, though not always, outside of the Haskell runtime).
module StackGraph.Handle
  ( Handle (..),
    nullHandle,
    isNull,
    emptyListHandle,
  )
where

import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)

-- | Handle values are small, strict values that can benefit from the @UNPACK@ pragma
-- so that they can be embedded directly into constructors.
newtype Handle t = Handle {unHandle :: Word32}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Storable)

-- | 'Handle' values that correspond to notion of "zero", "default", or "null" by convention
-- use this handle to represent the null value.
nullHandle :: Handle a
nullHandle = Handle 0

-- | Is this handle the null valeue?
isNull :: Handle a -> Bool
isNull (Handle h) = h == 0

-- | 'Handle' values that represent lists use this to represent the empty value.
emptyListHandle :: Handle a
emptyListHandle = Handle c_SG_LIST_EMPTY_HANDLE

foreign import capi "stack-graphs.h value SG_LIST_EMPTY_HANDLE" c_SG_LIST_EMPTY_HANDLE :: Word32
