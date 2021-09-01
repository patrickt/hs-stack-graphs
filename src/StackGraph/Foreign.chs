{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module StackGraph.Foreign where

import Debug.Trace
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import GHC.Records
import Foreign.Storable
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Foreign.C.String
import Data.Vector.Mutable (IOVector)
import Data.Vector.Mutable as V
import Data.Int
import Foreign
import Unsafe.Coerce
import Data.Coerce
import Data.Word

#include "stack-graphs.h"
#include "hs_stack_graphs.h"

{#context prefix = "sg_" #}

{#enum deque_direction as DequeDirection {underscoreToCase} with prefix = "deque_" deriving (Eq, Show) #}

{#enum node_kind as NodeKind {underscoreToCase} with prefix = "node_kind_" deriving (Eq, Show) #}

data PartialPathArena
data PartialPathDatabase
data PartialPathList

{#pointer *partial_path_arena as PartialPathArenaPtr -> PartialPathArena #}
{#pointer *partial_path_database as PartialPathDatabasePtr -> PartialPathDatabase #}
{#pointer *partial_path_list as PartialPathListPtr -> PartialPathList #}

data PathArena
data PathList

{#pointer *path_arena as PathArenaPtr -> PathArena #}
{#pointer *path_list as PathListPtr -> PathList #}

data StackGraph

{#pointer *stack_graph as StackGraphPtr -> StackGraph #}

data Symbol = Symbol { symbol :: CString, symbol_len :: CSize }
  deriving Show

{#pointer *symbol as SymbolPtr -> Symbol #}

instance Storable Symbol where
  sizeOf _ = {#sizeof symbol#}
  alignment _ = {#alignof symbol#}
  peek p = do
    bod <- {#get symbol->symbol #} p
    len <- {#get symbol->symbol_len #} p
    pure (Symbol bod (fromIntegral len))
  poke p (Symbol bod len) = do
    {#set symbol.symbol #} p bod
    {#set symbol.symbol_len #} p (fromIntegral len)

data Symbols = Symbols { symbols :: SymbolPtr, count :: CSize }
  deriving stock Show

{#pointer *symbols as SymbolsPtr -> Symbols #}

instance Storable Symbols where
  sizeOf _ = {#sizeof symbols#}
  alignment _ = {#alignof symbols#}
  peek p = do
    bod <- {#get symbols->symbols #} p
    len <- {#get symbols->count #} p
    pure (Symbols bod (fromIntegral len))
  poke p (Symbols bod len) = do
    {#set symbols.symbols #} p bod
    {#set symbols.count #} p (fromIntegral len)

type SymbolHandle = {#type symbol_handle#}

{#pointer *symbol_handle as SymbolHandlePtr -> SymbolHandle #}

data File = File { name :: CString, name_len :: CSize }

{#pointer *file as FilePtr -> File #}

instance Storable File where
  sizeOf _ = {#sizeof file#}
  alignment _ = {#alignof file#}
  peek p = do
    bod <- {#get file->name #} p
    len <- {#get file->name_len #} p
    pure (File bod (fromIntegral len))
  poke p (File bod len) = do
    {#set file.name #} p bod
    {#set file.name_len #} p (fromIntegral len)

data Files = Files { files :: FilePtr, count :: CSize }

instance Storable Files where
  sizeOf _ = {#sizeof files#}
  alignment _ = {#alignof files#}
  peek p = do
    bod <- {#get files->files #} p
    len <- {#get files->count #} p
    pure (Files bod (fromIntegral len))
  poke p (Files bod len) = do
    {#set files.files #} p bod
    {#set files.count #} p (fromIntegral len)

type FileHandle = {#type file_handle#}

data NodeId = NodeId { file :: FileHandle, local_id :: Word32 }

{#pointer *node_id as NodeIdPtr -> NodeId #}

instance Storable NodeId where
  sizeOf _ = {#sizeof node_id#}
  alignment _ = {#alignof node_id#}
  peek p = do
    bod <- {#get node_id->file #} p
    len <- {#get node_id->local_id #} p
    pure (NodeId bod (fromIntegral len))
  poke p (NodeId bod len) = do
    {#set node_id.file #} p bod
    {#set node_id.local_id #} p (fromIntegral len)

data Node = Node
  { nodeKind :: NodeKind
  , nodeId :: NodeId
  , symbol :: SymbolHandle
  , scope :: NodeId
  , isClickable :: Bool
  }

type NodeHandle = {#type node_handle#}

rootNodeHandle :: NodeHandle
rootNodeHandle = {#const SG_ROOT_NODE_HANDLE#}

jumpToNodeHandle :: NodeHandle
jumpToNodeHandle = {#const SG_JUMP_TO_NODE_HANDLE#}

-- TODO storable

data Edge = Edge
  { source :: NodeHandle
  , sink :: NodeHandle
  , precedence :: Int32
  }

-- TODO storable

type ScopeStackCellHandle = {#type scope_stack_cell_handle#}

newtype ScopeStack = ScopeStack { cells :: ScopeStackCellHandle }

data ScopedSymbol = ScopedSymbol { symbol :: SymbolHandle, scopes :: ScopeStack }

type SymbolStackCellHandle = {#type symbol_stack_cell_handle#}

data SymbolStackCell = SymbolStackCell { head :: ScopedSymbol, tail :: SymbolStackCellHandle }

{#pointer *symbol_stack_cell as SymbolStackCellPtr -> SymbolStackCell #}

data SymbolStackCells = SymbolStackCells { cells :: SymbolStackCellPtr, count :: CSize }

instance Storable SymbolStackCells where
  sizeOf _ = {#sizeof symbol_stack_cells#}
  alignment _ = {#alignof symbol_stack_cells#}
  peek p = do
    bod <- {#get symbol_stack_cells->cells #} p
    len <- {#get symbol_stack_cells->count #} p
    pure (SymbolStackCells bod (fromIntegral len))
  poke p (SymbolStackCells bod len) = do
    {#set symbol_stack_cells.cells #} p (castPtr bod)
    {#set symbol_stack_cells.count #} p (fromIntegral len)

data SymbolStack = SymbolStack { cells :: Ptr SymbolStackCellHandle, length :: CSize }

data ScopeStackCell = ScopeStackCell { head :: NodeHandle, tail :: ScopeStackCellHandle }

{#pointer *scope_stack_cell as ScopeStackCellPtr -> ScopeStackCell #}

data ScopeStackCells = ScopeStackCells { cells :: ScopeStackCellPtr, count :: CSize }

instance Storable ScopeStackCells where
  sizeOf _ = {#sizeof scope_stack_cells#}
  alignment _ = {#alignof scope_stack_cells#}
  peek p = do
    bod <- {#get scope_stack_cells->cells #} p
    len <- {#get scope_stack_cells->count #} p
    pure (ScopeStackCells (castPtr bod) (fromIntegral len))
  poke p (ScopeStackCells bod len) = do
    {#set scope_stack_cells.cells #} p (castPtr bod)
    {#set scope_stack_cells.count #} p (fromIntegral len)


data PathEdge = PathEdge { source_node_id :: NodeId, precedence :: Int32 }

type PathEdgeListCellHandle = {#type path_edge_list_cell_handle#}

data PathEdgeListCell = PathEdgeListCell
  { head :: PathEdge
  , tail :: PathEdgeListCellHandle
  , reversed :: PathEdgeListCellHandle
  }

{#pointer *path_edge_list_cell as PathEdgeListCellPtr -> PathEdgeListCell #}

data PathEdgeListCells = PathEdgeListCells { cells :: PathEdgeListCellPtr, count :: CSize }

instance Storable PathEdgeListCells where
  sizeOf _ = {#sizeof path_edge_list_cells #}
  alignment _ = {#alignof path_edge_list_cells #}
  peek p = do
    bod <- {#get path_edge_list_cells->cells #} p
    len <- {#get path_edge_list_cells->count #} p
    pure (PathEdgeListCells bod (fromIntegral len))
  poke p (PathEdgeListCells bod len) = do
    {#set path_edge_list_cells.cells #} p bod
    {#set path_edge_list_cells.count #} p (fromIntegral len)

data PathEdgeList = PathEdgeList
  { cells :: PathEdgeListCellHandle
  , direction :: DequeDirection
  , length :: CSize
  }

data EdgeList = EdgeList
  { cells :: PathEdgeListCellHandle
  , direction :: DequeDirection
  , length :: CSize
  }

data Path = Path
  { startNode :: NodeHandle
  , endNode :: NodeHandle
  , symbolStack :: SymbolStack
  , scopeStack :: ScopeStack
  , edges :: PathEdgeList
  }

type PartialScopeStackCellHandle = {#type partial_scope_stack_cell_handle#}

type ScopeStackVariable = {#type scope_stack_variable#}

data PartialScopeStack = PartialScopeStack
  { cells :: PartialScopeStackCellHandle
  , direction :: DequeDirection
  , variable :: ScopeStackVariable
  }

data PartialScopedSymbol = PartialScopedSymbol { symbol :: SymbolHandle, scopes :: PartialScopeStack }

type PartialSymbolStackCellHandle = {#type partial_symbol_stack_cell_handle#}

data PartialSymbolStackCell = PartialSymbolStackCell
  { head :: PartialScopedSymbol
  , tail :: PartialSymbolStackCellHandle
  , reversed :: PartialSymbolStackCellHandle
  }

{#pointer *partial_symbol_stack_cell as PartialSymbolStackCellPtr -> PartialSymbolStackCell #}

data PartialSymbolStackCells = PartialSymbolStackCells { cells :: PartialSymbolStackCellPtr, count :: CSize }

instance Storable PartialSymbolStackCells where
  sizeOf _ = {#sizeof partial_symbol_stack_cells #}
  alignment _ = {#alignof partial_symbol_stack_cells #}
  peek p = do
    bod <- {#get partial_symbol_stack_cells->cells #} p
    len <- {#get partial_symbol_stack_cells->count #} p
    pure (PartialSymbolStackCells (castPtr bod) (fromIntegral len))
  poke p (PartialSymbolStackCells bod len) = do
    {#set partial_symbol_stack_cells.cells #} p (castPtr bod)
    {#set partial_symbol_stack_cells.count #} p (fromIntegral len)

data PartialSymbolStack = PartialSymbolStack
  { cells :: PartialSymbolStackCellHandle
  , direction :: DequeDirection
  }

data PartialScopeStackCell = PartialScopeStackCell
  { head :: NodeHandle
  , tail :: PathEdgeListCellHandle
  , reversed :: PathEdgeListCellHandle
  }

{#pointer *partial_scope_stack_cell as PartialScopeStackCellPtr -> PartialScopeStackCell #}

data PartialScopeStackCells = PartialScopeStackCells { cells :: PartialScopeStackCellPtr, count :: CSize }

data PartialPathEdge = PartialPathEdge { sourceNode :: NodeId, precedence :: Int32 }

instance Storable PartialPathEdge where
  sizeOf _ = {#sizeof partial_path_edge#}
  alignment _ = {#alignof partial_path_edge#}
  peek p = do
    bod <- peekByteOff (castPtr p) 0
    len <- peekByteOff (castPtr p) {#offsetof partial_path_edge->precedence#}
    pure (PartialPathEdge bod len)
  poke p (PartialPathEdge bod len) = do
    pokeByteOff (castPtr p) 0 bod
    pokeByteOff (castPtr p) {#offsetof partial_path_edge->precedence#} len


type PartialPathEdgeListCellHandle = {#type partial_path_edge_list_cell_handle#}

data PartialPathEdgeListCell = PartialPathEdgeListCell
  { head :: PartialPathEdge
  , tail :: PartialPathEdgeListCellHandle
  , reversed :: PartialPathEdgeListCellHandle
  }

{#pointer *partial_path_edge_list_cell as PartialPathEdgeListCellPtr -> PartialPathEdgeListCell #}

instance Storable PartialPathEdgeListCell where
  sizeOf _ = {#sizeof partial_path_edge_list_cell#}
  alignment _ = {#alignof partial_path_edge_list_cell#}
  peek p = do
    bod <- peekByteOff (castPtr p) 0
    len <- peekByteOff (castPtr p) {#offsetof partial_path_edge_list_cell->tail#}
    rev <- peekByteOff (castPtr p) {#offsetof partial_path_edge_list_cell->reversed#}
    pure (PartialPathEdgeListCell bod len rev)
  poke p (PartialPathEdgeListCell bod len rev) = do
    pokeByteOff (castPtr p) 0 bod
    pokeByteOff (castPtr p) {#offsetof partial_path_edge_list_cell->tail#} len
    pokeByteOff (castPtr p) {#offsetof partial_path_edge_list_cell->reversed#} rev


data PartialPathEdgeListCells = PartialPathEdgeListCells { cells :: PartialPathEdgeListCellPtr, count :: CSize }

{#pointer *partial_path_edge_list_cells as PartialPathEdgeListCellsPtr -> PartialPathEdgeListCells #}

instance Storable PartialPathEdgeListCells where
  sizeOf _ = {#sizeof partial_path_edge_list_cells #}
  alignment _ = {#alignof partial_path_edge_list_cells #}
  peek p = do
    bod <- {#get partial_path_edge_list_cells->cells #} p
    len <- {#get partial_path_edge_list_cells->count #} p
    pure (PartialPathEdgeListCells (castPtr bod) (fromIntegral len))
  poke p (PartialPathEdgeListCells bod len) = do
    {#set partial_path_edge_list_cells.cells #} p (castPtr bod)
    {#set partial_path_edge_list_cells.count #} p (fromIntegral len)

data PartialPathEdgeList = PartialPathEdgeList
  { cells :: PartialPathEdgeListCellHandle
  , direction :: DequeDirection
  , length :: CSize
  }
data PartialPath = PartialPath
  { startNode :: NodeHandle
  , endNode :: NodeHandle
  , symbolStackPreconditicon :: PartialSymbolStack
  , symbolStackPostcondition :: PartialSymbolStack
  , scopeStackPrecondition :: PartialScopeStack
  , scopeStackPostcondition :: PartialScopeStack
  , edges :: PartialPathEdgeList
  }

data PartialPaths = PartialPaths { cells :: Ptr PartialPath, count :: CSize }

type PartialPathHandle = {#type partial_path_handle#}

data ForwardPathStitcher = ForwardPatchStitcher { previousPhasePath :: Ptr Path, previousPhasePathsLength :: CSize }

castPeek :: Storable b => Ptr () -> IO b
castPeek p = do
  val <- peek (castPtr p)
  free p
  return val

{#fun unsafe stack_graph_new as ^ {} -> `StackGraphPtr' #}
{#fun unsafe stack_graph_free as ^ {`StackGraphPtr'} -> `()' #}

{#fun unsafe path_arena_new as ^ {} -> `PathArenaPtr' #}
{#fun unsafe path_arena_free as ^ {`PathArenaPtr'} -> `()' #}

{#fun unsafe partial_path_arena_new as ^ {} -> `PartialPathArenaPtr' #}
{#fun unsafe partial_path_arena_free as ^ {`PartialPathArenaPtr'} -> `()' #}

{#fun unsafe partial_path_database_new as ^ {} -> `PartialPathDatabasePtr' #}
{#fun unsafe partial_path_database_free as ^ {`PartialPathDatabasePtr'} -> `()' #}

{#fun stack_graph_symbols_ptr as stackGraphSymbols {`StackGraphPtr', alloca- `Symbols' peek* } -> `()' #}

{#fun unsafe stack_graph_add_symbols as ^
  { id `StackGraphPtr',
    fromIntegral `CSize',
    castPtr `CString',
    castPtr `Ptr CSize',
    castPtr `SymbolHandlePtr'} -> `()' #}

{#fun unsafe stack_graph_files as ^ {`StackGraphPtr'} -> `Files' castPeek* #}

{#fun unsafe stack_graph_add_files as ^
  { id `StackGraphPtr',
    fromIntegral `CSize',
    castPtr `Ptr CString',
    castPtr `Ptr CSize',
    castPtr `Ptr FileHandle'} -> `()' #}


{#fun unsafe stack_graph_get_or_create_nodes as ^
  { id `StackGraphPtr',
    fromIntegral `CSize',
    castPtr `Ptr Node',
    castPtr `Ptr NodeHandle'} -> `()' #}

{#fun unsafe stack_graph_add_edges as ^
  { id `StackGraphPtr', fromIntegral `CSize', castPtr `Ptr Node'} -> `()' #}

{#fun unsafe path_arena_symbol_stack_cells as ^ { castPtr `PathArenaPtr'} -> `SymbolStackCells' castPeek* #}

{#fun unsafe path_arena_add_symbol_stacks as ^
  { id `PathArenaPtr',
    fromIntegral `CSize',
    castPtr `Ptr ScopedSymbol',
    castPtr `Ptr CSize',
    castPtr `Ptr SymbolStack'} -> `()' #}

{#fun unsafe path_arena_scope_stack_cells as ^ { castPtr `PathArenaPtr'} -> `ScopeStackCells' castPeek* #}

{#fun unsafe path_arena_add_scope_stacks as ^
  { id `PathArenaPtr',
    fromIntegral `CSize',
    castPtr `Ptr NodeHandle',
    castPtr `Ptr CSize',
    castPtr `Ptr ScopeStack'} -> `()' #}

{#fun unsafe path_arena_path_edge_list_cells as ^ { castPtr `PathArenaPtr'} -> `PathEdgeListCells' castPeek* #}

{#fun unsafe path_arena_add_path_edge_lists as ^
  { id `PathArenaPtr',
    fromIntegral `CSize',
    castPtr `Ptr PathEdge',
    castPtr `Ptr CSize',
    castPtr `Ptr PathEdgeList'} -> `()' #}

{#fun unsafe path_list_new as ^ {} -> `PathListPtr' #}
{#fun unsafe path_list_free as ^ {`PathListPtr'} -> `()' #}
{#fun unsafe path_list_count as ^ {`PathListPtr'} -> `CSize' fromIntegral #}
{#fun unsafe path_list_paths as ^ {`PathListPtr'} -> `Ptr Path' castPeek* #}

{#fun unsafe path_arena_find_all_complete_paths as ^
  { id `StackGraphPtr',
    id `PathArenaPtr',
    fromIntegral `CSize',
    castPtr `Ptr NodeHandle',
    castPtr `Ptr PathList'
  } -> `()' #}

{#fun unsafe partial_path_arena_partial_symbol_stack_cells as ^ { castPtr `PartialPathArenaPtr'} -> `PartialSymbolStackCells' castPeek* #}

{#fun unsafe partial_path_arena_add_partial_symbol_stacks as ^
  { id `PartialPathArenaPtr',
    fromIntegral `CSize',
    castPtr `Ptr PartialScopedSymbol',
    castPtr `Ptr CSize',
    castPtr `Ptr PartialSymbolStack'} -> `()' #}

{#fun unsafe partial_path_arena_add_partial_scope_stacks as ^
  { id `PartialPathArenaPtr',
    fromIntegral `CSize',
    castPtr `Ptr NodeHandle',
    castPtr `Ptr CSize',
    castPtr `Ptr ScopeStackVariable',
    castPtr `Ptr PartialSymbolStack'} -> `()' #}

{#fun partial_path_arena_partial_path_edge_list_cells_ptr as partialPathArenaPartialPathEdgeListCells
  { `PartialPathArenaPtr', alloca- `PartialPathEdgeListCells' peek* } -> `()' #}

{#fun unsafe partial_path_arena_add_partial_path_edge_lists as ^
  { id `PartialPathArenaPtr',
    fromIntegral `CSize',
    castPtr `Ptr PartialScopedSymbol',
    castPtr `Ptr CSize',
    castPtr `Ptr PartialSymbolStack'} -> `()' #}
