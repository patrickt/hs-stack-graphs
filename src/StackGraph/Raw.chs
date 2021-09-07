{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackGraph.Raw (StackGraph, stackGraphNew, stackGraphAddSymbols, stackGraphSymbols) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Foreign
import Foreign.C.Types
import Data.Coerce
import Data.List (genericLength)
import Data.String (IsString)

#include "stack-graphs.h"
#include "hs_stack_graphs.h"

{#context prefix = "sg_" #}

{#typedef size_t CSize#}

{#pointer *stack_graph as StackGraph foreign finalizer stack_graph_free newtype #}
{#fun stack_graph_new as ^ {} -> `StackGraph' #}

{#pointer *partial_path_arena as PartialPathArena foreign finalizer partial_path_arena_free newtype #}

newtype Symbol = Symbol { unSymbol :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (IsString)

{#pointer *symbol as SymbolPtr -> Symbol #}

type SymbolHandle = {#type symbol_handle#}

instance Storable Symbol where
  sizeOf _ = {#sizeof symbol #}
  alignment _ = {#alignof symbol#}
  peek p = do
    str <- {#get symbol->symbol#} p
    len <- {#get symbol->symbol_len#} p
    Symbol <$> B.packCStringLen (str, fromIntegral len)
  poke p (Symbol bytes) = B.useAsCStringLen bytes $ \(str, len) -> do
    {#set symbol->symbol#} p str
    {#set symbol->symbol_len#} p (fromIntegral len)

-- todo: write peekVector
newtype Symbols = Symbols { unSymbols :: [Symbol] }
{#pointer *symbols as SymbolsPtr -> Symbols #}

instance Storable Symbols where
  sizeOf _ = {#sizeof symbols#}
  alignment _ = {#alignof symbols#}
  peek p = do
    sym <- {#get symbols.symbols#} p
    len <- {#get symbols.count#} p
    Symbols <$> peekArray (fromIntegral len) sym
  poke p (Symbols syms) = withArray syms $ \sp -> do
    {#set symbols->count#} p (fromIntegral (length syms))
    pokeArray sp syms

raw_stack_graph_add_symbols :: Ptr StackGraph -> CSize -> Ptr CChar -> Ptr CSize -> Ptr SymbolHandle -> IO ()
raw_stack_graph_add_symbols = {# call stack_graph_add_symbols #}

newtype Handle a = Handle Word32
  deriving stock (Eq, Show)

stackGraphAddSymbols :: StackGraph -> [Symbol] -> IO [Handle Symbol]
stackGraphAddSymbols sg syms = withStackGraph sg $ \sgp -> do
  let count = length syms
  let cSymCount :: CSize = genericLength syms
  let concatted :: ByteString = mconcat (coerce syms)
  let lengths :: [CSize] = fmap (fromIntegral . B.length . unSymbol) syms
  B.useAsCString concatted $ \csyms -> do
    withArray @CSize lengths $ \lengthsptr -> do
      allocaArray @SymbolHandle count $ \handleptr -> do
        raw_stack_graph_add_symbols sgp cSymCount csyms lengthsptr handleptr
        peeked <- peekArray count handleptr
        pure (fmap coerce peeked)

raw_stack_graph_symbols_ptr :: Ptr StackGraph -> Ptr Symbols -> IO ()
raw_stack_graph_symbols_ptr = {# call stack_graph_symbols_ptr #}

stackGraphSymbols :: StackGraph -> IO [Symbol]
stackGraphSymbols sg = withStackGraph sg $ \sgp -> do
  alloca @Symbols $ \symsptr -> do
    raw_stack_graph_symbols_ptr sgp symsptr
    x <- peek symsptr
    pure (unSymbols x)
