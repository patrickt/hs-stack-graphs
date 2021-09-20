{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module StackGraph.Handle (Handle (..), nullHandle) where

import Foreign.Storable
import Data.Word
import GHC.Generics (Generic)

newtype Handle t = Handle {unHandle :: Word32}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Storable)

nullHandle :: Handle a
nullHandle = Handle 0

isNull :: Handle a -> Bool
isNull (Handle h) = h == 0
