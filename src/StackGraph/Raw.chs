{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackGraph.Raw
  ( StackGraph,
    stackGraphNew,
    Symbol (..),
    stackGraphAddSymbols,
    stackGraphSymbolsRaw
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Foreign
import Foreign.C.Types
import Foreign.C.String (CString)
import Data.Coerce
import Control.Exception (assert, finally)
import Data.List (genericLength)
import Data.String (IsString)
import Debug.Trace

#include "stack-graphs.h"
#include "hs_stack_graphs.h"

{#context prefix = "sg_" #}

{#typedef size_t CSize#}

{#pointer *stack_graph as StackGraph foreign finalizer stack_graph_free newtype #}
{#fun stack_graph_new as ^ {} -> `StackGraph' #}

newtype Symbol = Symbol { unSymbol :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (IsString)

{#pointer *symbol as SymbolPtr -> Symbol #}

instance Storable Symbol where
  sizeOf _ = {#sizeof symbol #}
  alignment _ = {#alignof symbol#}
  peek p = do
    str <- {#get symbol->symbol#} p
    len <- fromIntegral <$> {#get symbol->symbol_len#} p
    Symbol <$> B.packCStringLen (str, len)
  poke p (Symbol bytes) = B.useAsCStringLen bytes $ \(str, len) -> do
    {#set symbol->symbol#} p str
    {#set symbol->symbol_len#} p (fromIntegral len)

type SymbolHandle = {#type symbol_handle#}

raw_stack_graph_add_symbols :: Ptr StackGraph -> CSize -> Ptr CChar -> Ptr CSize -> Ptr SymbolHandle -> IO ()
raw_stack_graph_add_symbols = {# call stack_graph_add_symbols #}

newtype Handle a = Handle Word32
  deriving (Eq, Show)

stackGraphAddSymbols :: StackGraph -> [Symbol] -> IO [Handle Symbol]
stackGraphAddSymbols sg syms = withStackGraph sg $ \sgp -> do
  let count = length syms
  let cSymCount :: CSize = fromIntegral count
  let concatted :: ByteString = mconcat (coerce syms)
  let lengths :: [CSize] = fmap (fromIntegral . B.length . unSymbol) syms
  B.useAsCString concatted $ \csyms -> do
    withArray @CSize lengths $ \lengthsptr -> do
      allocaArray @SymbolHandle count $ \handleptr -> do
        raw_stack_graph_add_symbols sgp cSymCount csyms lengthsptr handleptr
        peeked <- peekArray count handleptr
        pure (fmap coerce peeked)

{#pointer *symbols as Symbols foreign finalizer symbols_free newtype #}

{#fun stack_graph_symbols_new as ^ {`StackGraph'} -> `Symbols' #}
{#fun symbols_count as ^ {`Symbols'} -> `CSize' #}
{#fun symbols_copy as ^ {`Symbols', id `Ptr CString'} -> `()' #}

stackGraphSymbolsRaw :: StackGraph -> IO [Symbol]
stackGraphSymbolsRaw sg = do
  x <- stackGraphSymbolsNew sg
  slen <- symbolsCount x
  withSymbols x $ \xp -> do
    let len :: CSize = fromIntegral slen
    allocaArray @CString (fromIntegral slen) $ \allsyms -> do
      symbolsCopy x allsyms
      strings <- peekArray (fromIntegral slen) allsyms
      item <- traverse (fmap Symbol . B.packCString) strings
      pure (drop 1 item)
