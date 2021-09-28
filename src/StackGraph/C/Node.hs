{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveAnyClass #-}
module StackGraph.C.Node
  ( NodeId (..)
  , defaultNodeId
  , rootNodeId
  , jumpToNodeId
  , Node (..)
  , Nodes
  ) where

import StackGraph.C.File (File)
import StackGraph.Handle
import GHC.Generics (Generic)
import Foreign.Storable.Generic
import Foreign.C
import Data.Word
import StackGraph.C.Symbol (Symbol)
import StackGraph.C.Slab (Slab)

data NodeId = NodeId
  { file :: Handle File
  , localId :: Handle NodeId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GStorable)

foreign import capi "stack-graphs.h value SG_ROOT_NODE_ID" root_node_id :: Word32
foreign import capi "stack-graphs.h value SG_JUMP_TO_NODE_ID" jump_to_node_id :: Word32

defaultNodeId :: NodeId
defaultNodeId = NodeId (Handle 0) (Handle 0)

rootNodeId :: NodeId
rootNodeId = NodeId (Handle 0) (Handle root_node_id)

jumpToNodeId :: NodeId
jumpToNodeId = NodeId (Handle 0) (Handle jump_to_node_id)

data Node = Node
  { nodeKind :: CInt
  , nodeId :: NodeId
  , nodeSymbol :: Handle Symbol
  , nodeScope :: NodeId
  , nodeIsClickable :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GStorable)

type Nodes = Slab Node
