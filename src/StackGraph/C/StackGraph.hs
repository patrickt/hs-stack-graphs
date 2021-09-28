{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module StackGraph.C.StackGraph
  ( StackGraph
  , withStackGraph
  , new
  , addFiles
  , addSymbols
  , files
  , getOrCreateNodes
  , nodes
  , symbols
  , addEdges
  )
  where

import Foreign hiding (new)
import Data.Traversable
import Foreign.C
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Foreign.Storable
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.Generics (Generic)
import Control.Exception
import Foreign.Storable.Generic
import Data.Foldable (fold)
import StackGraph.Handle
import StackGraph.C.Slab (Slab)
import StackGraph.C.Node (Node, Nodes)
import StackGraph.C.Symbol (Symbol, Symbols)
import StackGraph.C.Symbol qualified as Symbol
import StackGraph.C.File (File, Files)
import StackGraph.C.File qualified as File
import StackGraph.C.Slab qualified as Slab
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Generic qualified as Vector
import Control.Monad
import StackGraph.C.Edge (Edge)
import Data.List (genericLength)

-- * Stack graphs

newtype {-# CTYPE "stack-graphs.h" "struct sg_stack_graph" #-} StackGraph =
  StackGraph {unStackGraph :: ForeignPtr StackGraph}

foreign import capi unsafe "stack-graphs.h sg_stack_graph_new"
  sg_stack_graph_new :: IO (Ptr StackGraph)

foreign import capi unsafe "stack-graphs.h &sg_stack_graph_free"
  sg_stack_graph_free :: FinalizerPtr StackGraph

withStackGraph :: StackGraph -> (Ptr StackGraph -> IO b) -> IO b
withStackGraph (StackGraph fptr) = withForeignPtr fptr

new :: IO StackGraph
new = do
  sg <- sg_stack_graph_new
  fp <- newForeignPtr sg_stack_graph_free sg
  pure (StackGraph fp)

peekVector :: forall a. (Storable a) => Int -> Ptr a -> IO (VS.Vector a)
peekVector size ptr
  | size <= 0 = return VS.empty
  | otherwise = do
    let n = size * sizeOf (undefined :: a)
    fp <- mallocPlainForeignPtrBytes n
    withForeignPtr fp $ \p -> copyBytes p ptr n
    return $ VS.unsafeFromForeignPtr0 fp size


-- * Adding symbols to stack graphs

foreign import capi unsafe "stack-graphs.h sg_stack_graph_add_symbols"
  sg_stack_graph_add_symbols :: Ptr StackGraph -> CSize -> CString -> Ptr CSize -> Ptr (Handle Symbol) -> IO ()

addSymbols :: StackGraph -> [ByteString] -> IO (VS.Vector (Handle Symbol))
addSymbols sg syms =
  withStackGraph sg \sgptr ->
    B.useAsCString concatted \symptr ->
      withArray @CSize lengths \lenptr -> do
        allocaArray @(Handle Symbol) count \hdlptr -> do
          sg_stack_graph_add_symbols sgptr (fromIntegral count) symptr lenptr hdlptr
          peekVector count hdlptr
  where
    concatted :: ByteString = fold syms
    lengths :: [CSize] = fmap (fromIntegral . B.length) syms
    count :: Int = length syms

foreign import capi unsafe "hs_stack_graphs.h sg_stack_graph_symbols_ptr"
  sg_stack_graph_symbols_ptr :: Ptr StackGraph -> Ptr Symbols -> IO ()

symbols :: StackGraph -> IO [ByteString]
symbols sg =
  withStackGraph sg \sgptr -> do
    alloca @Symbols \symsptr -> do
      sg_stack_graph_symbols_ptr sgptr symsptr
      allsyms <- peek symsptr >>= Slab.contents
      for (drop 1 allsyms) \sym ->
        B.packCStringLen (Symbol.symbol sym, fromIntegral (Symbol.symbol_len sym))

-- * Files

foreign import capi unsafe "stack-graphs.h sg_stack_graph_add_files"
  sg_stack_graph_add_files :: Ptr StackGraph -> CSize -> CString -> Ptr CSize -> Ptr (Handle File) -> IO ()

foreign import capi unsafe "hs_stack_graphs.h sg_stack_graph_files_ptr"
  sg_stack_graph_files_ptr :: Ptr StackGraph -> Ptr Files -> IO ()

-- * Adding files to stack graphs

addFiles :: StackGraph -> [ByteString] -> IO (VS.Vector (Handle File))
addFiles sg syms = do
  withStackGraph sg \sgptr ->
    B.useAsCString concatted \symptr ->
      withArray @CSize lengths \lenptr ->
        allocaArray @(Handle File) count \hdlptr -> do
          sg_stack_graph_add_files sgptr (fromIntegral count) symptr lenptr hdlptr
          peekVector count hdlptr
  where
    concatted :: ByteString = fold syms
    lengths :: [CSize] = fmap (fromIntegral . B.length) syms
    count :: Int = length syms

files :: StackGraph -> IO (V.Vector ByteString)
files sg =
  withStackGraph sg \sgptr -> do
    alloca @Files \symsptr -> do
      sg_stack_graph_files_ptr sgptr symsptr
      allsyms <- Slab.contents' =<< peek symsptr
      for (Vector.convert (Vector.drop 1 allsyms)) \sym ->
        B.packCStringLen (File.name sym, fromIntegral (File.name_len sym))

-- * Nodes

foreign import capi unsafe "stack-graphs.h sg_stack_graph_get_or_create_nodes"
  sg_stack_graph_get_or_create_nodes :: Ptr StackGraph -> CSize -> Ptr Node -> Ptr (Handle Node) -> IO ()

foreign import capi unsafe "hs_stack_graphs.h sg_stack_graph_nodes_ptr"
  sg_stack_graph_nodes_ptr :: Ptr StackGraph -> Ptr Nodes -> IO ()

getOrCreateNodes :: StackGraph -> [Node] -> IO [Handle Node]
getOrCreateNodes sg nodes =
  withStackGraph sg \sgptr -> do
    withArray nodes \nodesptr -> do
      allocaArray @(Handle Node) (fromIntegral count) $ do
        sg_stack_graph_get_or_create_nodes sgptr (fromIntegral count) nodesptr
        peekArray count
  where
    count :: Int = length nodes

nodes :: StackGraph -> IO (Slab Node)
nodes sg =
  withStackGraph sg \sgptr -> do
    alloca @Nodes $ do
      sg_stack_graph_nodes_ptr sgptr
      peek

-- * Edges

foreign import capi unsafe "stack-graphs.h sg_stack_graph_add_edges"
  sg_stack_graph_edges :: Ptr StackGraph -> CSize -> Ptr Edge -> IO ()

addEdges :: StackGraph -> [Edge] -> IO ()
addEdges sg edges =
  withStackGraph sg \sgptr ->
    withArray edges $
      sg_stack_graph_edges sgptr (genericLength edges)
