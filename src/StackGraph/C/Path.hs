{-# LANGUAGE ImportQualifiedPost #-}
module StackGraph.C.Path where

import StackGraph.Handle (Handle)
import StackGraph.C.Node (Node)
import StackGraph.C.Symbol qualified as Symbol
import StackGraph.C.Scope qualified as Scope
import StackGraph.C.Path.Edge qualified as Path.Edge
import Foreign

data Path = Path
  { startNode :: Handle Node
  , endNode :: Handle Node
  , symbolStack :: Symbol.Stack
  , scopeStack :: Scope.Stack
  , edges :: Path.Edge.List
  }

newtype {-# CTYPE "stack-graphs.h" "struct sg_path_arena" #-} Arena =
  Arena {unArena :: ForeignPtr Arena}
