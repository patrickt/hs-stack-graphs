-- | Manages the state of a collection of partial paths to be used in the path-stitching algorithm.
module StackGraph.C.Partial.Path.Arena
  ( Arena
  , withArena
  , newArena
  ) where

import Foreign
import Foreign.C

newtype {-# CTYPE "stack-graphs.h" "struct sg_partial_path_arena" #-} Arena = Arena (ForeignPtr Arena)

foreign import capi unsafe "stack-graphs.h sg_partial_path_arena_new"
  sg_partial_path_arena_new :: IO (Ptr Arena)

foreign import capi unsafe "stack-graphs.h &sg_partial_path_arena_free"
  sg_partial_path_arena_free :: FinalizerPtr Arena

withArena :: Arena -> (Ptr Arena -> IO b) -> IO b
withArena (Arena fptr) = withForeignPtr fptr
{-# INLINE withArena #-}

newArena :: IO Arena
newArena = do
  sg <- sg_partial_path_arena_new
  fp <- newForeignPtr sg_partial_path_arena_free sg
  pure (Arena fp)
