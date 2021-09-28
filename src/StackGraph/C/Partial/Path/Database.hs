{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

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
