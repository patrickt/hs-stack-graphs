{-# LANGUAGE Strict #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module StackGraph.C.Edge where

import StackGraph.Handle
import StackGraph.C.Node
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Data.Int

data Edge = Edge
  { source :: Handle Node
  , sink :: Handle Node
  , precedence :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GStorable)
