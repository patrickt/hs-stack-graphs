{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

-- | Contains a "database" of partial paths.
--
-- This type is meant to be a lazily loaded "view" into a proper storage layer.  During the
-- path-stitching algorithm, we repeatedly try to extend a currently incomplete path with any
-- partial paths that are compatible with it.  For large codebases, or projects with a large
-- number of dependencies, it can be prohibitive to load in _all_ of the partial paths up-front.
-- We've written the path-stitching algorithm so that you have a chance to only load in the
-- partial paths that are actually needed, placing them into a 'Database'
-- as they're needed.
module StackGraph.C.Partial.Path.Database where

import StackGraph.C.Partial.Path

import Foreign
import Foreign.C
import StackGraph.C.StackGraph (StackGraph)
import StackGraph.C.StackGraph qualified as StackGraph
import StackGraph.C.Partial.Path.Arena qualified as Partial.Path
import StackGraph.C.Partial.Path qualified as Partial
import StackGraph.Handle
import Data.List (genericLength)

newtype {-# CTYPE "stack-graphs.h" "struct sg_partial_path_database" #-} Database =
  Database {unDatabase :: ForeignPtr Database}

foreign import capi unsafe "stack-graphs.h sg_partial_path_database_new"
  sg_partial_path_database_new :: IO (Ptr Database)

foreign import capi unsafe "stack-graphs.h &sg_partial_path_database_free"
  sg_partial_path_database_free :: FinalizerPtr Database


foreign import capi unsafe "stack-graphs.h sg_partial_path_database_add_partial_paths"
  sg_partial_path_database_add_partial_paths :: Ptr StackGraph -> Ptr Partial.Path.Arena -> Ptr Database -> CSize -> Ptr Partial.Path -> Ptr (Handle Partial.Path) -> IO ()

withDatabase :: Database -> (Ptr Database -> IO b) -> IO b
withDatabase (Database fptr) = withForeignPtr fptr
{-# INLINE withDatabase #-}

newDatabase :: IO Database
newDatabase = do
  sg <- sg_partial_path_database_new
  fp <- newForeignPtr sg_partial_path_database_free sg
  pure (Database fp)

addPartialPaths :: StackGraph -> Partial.Path.Arena -> Database -> [Partial.Path] -> IO [Handle Partial.Path]
addPartialPaths sg ar db paths =
  StackGraph.withStackGraph sg \sgptr ->
    Partial.Path.withArena ar \arptr ->
      withDatabase db \dbptr ->
        withArray paths \pathsptr ->
          allocaArray @(Handle Partial.Path) (length paths) $ do
            sg_partial_path_database_add_partial_paths sgptr arptr dbptr (genericLength paths) pathsptr
            peekArray (length paths)

-- partialPaths :: StackGraph -> IO (Slab Path)
-- partialPath db =
--   withDatabase db \dbptr -> do
--     alloca @Paths $ do
--       sg_stack_graph_nodes_ptr sgptr
--       peek
