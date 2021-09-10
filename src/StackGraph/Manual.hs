{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CApiFFI #-}
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

-- * Handles

newtype Handle t = Handle {unHandle :: Word32}
  deriving stock (Eq, Show)
  deriving newtype (Storable)

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

data {-# CTYPE "stack-graphs.h" "struct sg_symbols" #-} Symbols = Symbols { symbols :: Ptr Symbol, count :: CSize }
  deriving stock (Eq, Show, Generic)
  deriving anyclass GStorable

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
      temp <- peek symsptr
      allsyms <- peekArray (fromIntegral (count temp)) (symbols temp)
      for (drop 1 allsyms) \sym ->
        B.packCStringLen (symbol sym, fromIntegral (symbol_len sym))

-- * Files

data {-# CTYPE "stack-graphs.h" "struct sg_file" #-} File = File { name :: CString, name_len :: CSize }
  deriving stock (Generic)
  deriving anyclass GStorable

data {-# CTYPE "stack-graphs.h" "struct sg_files" #-} Files = Files { files :: Ptr File, fileCount :: CSize }
  deriving stock (Eq, Show, Generic)
  deriving anyclass GStorable

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
      temp <- peek symsptr
      allsyms <- peekArray (fromIntegral (fileCount temp)) (files temp)
      for (drop 1 allsyms) \sym ->
        B.packCStringLen (name sym, fromIntegral (name_len sym))
