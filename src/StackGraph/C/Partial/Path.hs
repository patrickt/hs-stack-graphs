module StackGraph.C.Partial.Path where

import StackGraph.C.Partial.Scope qualified as Scope
import StackGraph.C.Partial.Symbol qualified as Symbol
import StackGraph.C.Partial.Path.Edge qualified as Edge
import StackGraph.Handle
import StackGraph.C.Node (Node)
import Foreign
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

data Path = Path
  { startNode :: Handle Node
  , endNode :: Handle Node
  , symbolStackPrecondition :: Symbol.Stack
  , symbolStackPostcondition :: Symbol.Stack
  , scopeStackPrecondition :: Scope.Stack
  , scopeStackPostcondition :: Scope.Stack
  , edges :: Edge.List
  }
  deriving stock Generic
  deriving anyclass (GStorable)

newtype {-# CTYPE "stack-graphs.h" "struct sg_partial_path_list" #-} List =
  Arena {unArena :: ForeignPtr List}


-- struct sg_partial_path {
--     sg_node_handle start_node;
--     sg_node_handle end_node;
--     struct sg_partial_symbol_stack symbol_stack_precondition;
--     struct sg_partial_symbol_stack symbol_stack_postcondition;
--     struct sg_partial_scope_stack scope_stack_precondition;
--     struct sg_partial_scope_stack scope_stack_postcondition;
--     struct sg_partial_path_edge_list edges;
-- }
