{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- TODO: stop hardcoding SG_LIST_EMPTY_HANDLE

newtype Handle t = Handle {unHandle :: Word32}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Storable)

nullHandle :: Handle a
nullHandle = Handle 0

isNull :: Handle a -> Bool
isNull (Handle h) = h == 0

emptyListHandle :: Handle a
emptyListHandle = Handle c_SG_LIST_EMPTY_HANDLE

foreign import capi "stack-graphs.h value SG_LIST_EMPTY_HANDLE" c_SG_LIST_EMPTY_HANDLE :: Word32
