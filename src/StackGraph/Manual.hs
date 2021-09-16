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

module StackGraph.Manual where

import Foreign
import Data.Traversable
import Foreign.C
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Foreign.Storable
import GHC.Generics (Generic)
import Foreign.Storable.Generic
import Data.Foldable (fold)
import StackGraph.Handle

-- * Stack graphs

newtype {-# CTYPE "stack-graphs.h" "struct sg_stack_graph" #-} StackGraph =
  StackGraph {unStackGraph :: ForeignPtr StackGraph}

foreign import capi unsafe "stack-graphs.h sg_stack_graph_new"
  sg_stack_graph_new :: IO (Ptr StackGraph)

foreign import capi unsafe "stack-graphs.h &sg_stack_graph_free"
  sg_stack_graph_free :: FinalizerPtr StackGraph

withStackGraph :: StackGraph -> (Ptr StackGraph -> IO b) -> IO b
withStackGraph (StackGraph fptr) = withForeignPtr fptr

stackGraphNew :: IO StackGraph
stackGraphNew = do
  sg <- sg_stack_graph_new
  fp <- newForeignPtr sg_stack_graph_free sg
  pure (StackGraph fp)

-- * Symbols

data {-# CTYPE "stack-graphs.h" "struct sg_symbol" #-} Symbol = Symbol { symbol :: CString, symbol_len :: CSize }
  deriving stock (Eq, Show, Generic)
  deriving anyclass GStorable

data List a = List { buffer :: Ptr a, count :: CSize }
  deriving stock (Eq, Show, Generic)
  deriving anyclass GStorable

peekList :: Storable a => Ptr (List a) -> IO [a]
peekList p = do
  List buf siz <- peek p
  peekArray (fromIntegral siz) buf

type Symbols = List Symbol

-- * Adding symbols to stack graphs

foreign import capi unsafe "stack-graphs.h sg_stack_graph_add_symbols"
  sg_stack_graph_add_symbols :: Ptr StackGraph -> CSize -> CString -> Ptr CSize -> Ptr (Handle Symbol) -> IO ()

stackGraphAddSymbols :: StackGraph -> [ByteString] -> IO [Handle Symbol]
stackGraphAddSymbols sg syms =
  withStackGraph sg \sgptr ->
    B.useAsCString concatted \symptr ->
      withArray @CSize lengths \lenptr ->
        allocaArray @(Handle Symbol) count \hdlptr -> do
          sg_stack_graph_add_symbols sgptr (fromIntegral count) symptr lenptr hdlptr
          -- TODO FIXME: invalid nodes can be null
          peekArray count hdlptr
  where
    concatted :: ByteString = fold syms
    lengths :: [CSize] = fmap (fromIntegral . B.length) syms
    count :: Int = length syms

foreign import capi unsafe "hs_stack_graphs.h sg_stack_graph_symbols_ptr"
  sg_stack_graph_symbols_ptr :: Ptr StackGraph -> Ptr Symbols -> IO ()

stackGraphSymbols :: StackGraph -> IO [ByteString]
stackGraphSymbols sg =
  withStackGraph sg \sgptr -> do
    alloca @Symbols \symsptr -> do
      sg_stack_graph_symbols_ptr sgptr symsptr
      allsyms <- peekList symsptr
      for (drop 1 allsyms) \sym ->
        B.packCStringLen (symbol sym, fromIntegral (symbol_len sym))

-- * Files

data {-# CTYPE "stack-graphs.h" "struct sg_file" #-} File = File { name :: CString, name_len :: CSize }
  deriving stock (Generic)
  deriving anyclass GStorable

type Files = List File

foreign import capi unsafe "stack-graphs.h sg_stack_graph_add_files"
  sg_stack_graph_add_files :: Ptr StackGraph -> CSize -> CString -> Ptr CSize -> Ptr (Handle File) -> IO ()

foreign import capi unsafe "hs_stack_graphs.h sg_stack_graph_files_ptr"
  sg_stack_graph_files_ptr :: Ptr StackGraph -> Ptr Files -> IO ()

-- * Adding files to stack graphs

stackGraphAddFiles :: StackGraph -> [ByteString] -> IO [Handle File]
stackGraphAddFiles sg syms =
  withStackGraph sg \sgptr ->
    B.useAsCString concatted \symptr ->
      withArray @CSize lengths \lenptr ->
        allocaArray @(Handle File) count \hdlptr -> do
          sg_stack_graph_add_files sgptr (fromIntegral count) symptr lenptr hdlptr
          peekArray count hdlptr
  where
    concatted :: ByteString = fold syms
    lengths :: [CSize] = fmap (fromIntegral . B.length) syms
    count :: Int = length syms

stackGraphFiles :: StackGraph -> IO [ByteString]
stackGraphFiles sg =
  withStackGraph sg \sgptr -> do
    alloca @Files \symsptr -> do
      sg_stack_graph_files_ptr sgptr symsptr
      allsyms <- peekList symsptr
      for (drop 1 allsyms) \sym ->
        B.packCStringLen (name sym, fromIntegral (name_len sym))

-- * Nodes

data NodeId = NodeId
  { file :: Handle File
  , localId :: Handle ()
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GStorable)

data Node = Node
  { nodeKind :: CInt
  , nodeId :: NodeId
  , nodeSymbol :: Handle Symbol
  , nodeScope :: NodeId
  , nodeIsClickable :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GStorable)


type Nodes = List Node

foreign import capi unsafe "stack-graphs.h sg_stack_graph_get_or_create_nodes"
  sg_stack_graph_get_or_create_nodes :: Ptr StackGraph -> CSize -> Ptr Node -> Ptr (Handle Node) -> IO ()

foreign import capi unsafe "hs_stack_graphs.h sg_stack_graph_nodes_ptr"
  sg_stack_graph_nodes_ptr :: Ptr StackGraph -> Ptr Nodes -> IO ()

stackGraphGetOrCreateNodes :: StackGraph -> [Node] -> IO [Handle Node]
stackGraphGetOrCreateNodes sg nodes =
  withStackGraph sg \sgptr -> do
    withArray nodes \nodesptr -> do
      allocaArray @(Handle Node) (fromIntegral count) \hdlptr -> do
        sg_stack_graph_get_or_create_nodes sgptr (fromIntegral count) nodesptr hdlptr
        peekArray count hdlptr
  where
    count :: Int = length nodes

stackGraphNodes :: StackGraph -> IO [Node]
stackGraphNodes sg =
  withStackGraph sg \sgptr -> do
    alloca @Nodes \nodesptr -> do
      sg_stack_graph_nodes_ptr sgptr nodesptr
      peekList nodesptr
