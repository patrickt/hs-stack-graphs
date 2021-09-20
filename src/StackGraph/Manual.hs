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
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.Generics (Generic)
import Control.Exception
import Foreign.Storable.Generic
import Data.Foldable (fold)
import StackGraph.Handle
import StackGraph.C.Arena (Arena)
import StackGraph.C.Arena qualified as Arena
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Generic qualified as Vector
import Control.Monad

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

peekVector :: forall a. (Storable a) => Int -> Ptr a -> IO (VS.Vector a)
peekVector size ptr
  | size <= 0 = return VS.empty
  | otherwise = do
    let n = size * sizeOf (undefined :: a)
    fp <- mallocPlainForeignPtrBytes n
    withForeignPtr fp $ \p -> copyBytes p ptr n
    return $ VS.unsafeFromForeignPtr0 fp size


-- * Symbols

data {-# CTYPE "stack-graphs.h" "struct sg_symbol" #-} Symbol = Symbol { symbol :: CString, symbol_len :: CSize }
  deriving stock (Eq, Show, Generic)
  deriving anyclass GStorable

type Symbols = Arena Symbol

-- * Adding symbols to stack graphs

foreign import capi unsafe "stack-graphs.h sg_stack_graph_add_symbols"
  sg_stack_graph_add_symbols :: Ptr StackGraph -> CSize -> CString -> Ptr CSize -> Ptr (Handle Symbol) -> IO ()

stackGraphAddSymbols :: StackGraph -> [ByteString] -> IO (VS.Vector (Handle Symbol))
stackGraphAddSymbols sg syms =
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

stackGraphSymbols :: StackGraph -> IO [ByteString]
stackGraphSymbols sg =
  withStackGraph sg \sgptr -> do
    alloca @Symbols \symsptr -> do
      sg_stack_graph_symbols_ptr sgptr symsptr
      allsyms <- peek symsptr >>= Arena.contents
      for (drop 1 allsyms) \sym ->
        B.packCStringLen (symbol sym, fromIntegral (symbol_len sym))

-- * Files

data {-# CTYPE "stack-graphs.h" "struct sg_file" #-} File = File { name :: CString, name_len :: CSize }
  deriving stock (Generic)
  deriving anyclass GStorable

type Files = Arena File

foreign import capi unsafe "stack-graphs.h sg_stack_graph_add_files"
  sg_stack_graph_add_files :: Ptr StackGraph -> CSize -> CString -> Ptr CSize -> Ptr (Handle File) -> IO ()

foreign import capi unsafe "hs_stack_graphs.h sg_stack_graph_files_ptr"
  sg_stack_graph_files_ptr :: Ptr StackGraph -> Ptr Files -> IO ()

-- * Adding files to stack graphs

stackGraphAddFiles :: StackGraph -> [ByteString] -> IO (VS.Vector (Handle File))
stackGraphAddFiles sg syms = do
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

stackGraphFiles :: StackGraph -> IO (V.Vector ByteString)
stackGraphFiles sg =
  withStackGraph sg \sgptr -> do
    alloca @Files \symsptr -> do
      sg_stack_graph_files_ptr sgptr symsptr
      allsyms <- Arena.contents' =<< peek symsptr
      for (Vector.convert (Vector.drop 1 allsyms)) \sym ->
        B.packCStringLen (name sym, fromIntegral (name_len sym))

-- * Nodes

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


type Nodes = Arena Node

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

stackGraphNodes :: StackGraph -> IO (VS.Vector Node)
stackGraphNodes sg =
  withStackGraph sg \sgptr -> do
    alloca @Nodes \nodesptr -> do
      sg_stack_graph_nodes_ptr sgptr nodesptr
      Arena.contents' =<< peek nodesptr
