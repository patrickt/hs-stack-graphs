{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

data PathList
data PathArena

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

{#pointer *node as NodePtr -> Node #}

type NodeHandle = {#type node_handle#}

{#pointer *node_handle as NodeHandlePtr -> NodeHandle #}

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

type ScopeStackCellHandle = {#type scope_stack_cell_handle#}

newtype ScopeStack = ScopeStack { cells :: ScopeStackCellHandle }
  deriving newtype Storable

data ScopedSymbol = ScopedSymbol { symbol :: SymbolHandle, scopes :: ScopeStack }

instance Storable ScopedSymbol where
  sizeOf _ = {#sizeof scoped_symbol#}
  alignment _ = {#alignof scoped_symbol#}
  peek p = do
    bod <- {#get scoped_symbol->symbol #} p
    len <- peekByteOff (castPtr p) {#offsetof scoped_symbol->scopes#}
    pure (ScopedSymbol bod len)
  poke p (ScopedSymbol bod len) = do
    poke (castPtr p) bod
    pokeByteOff (castPtr p) {#offsetof scoped_symbol->scopes#} len

type SymbolStackCellHandle = {#type symbol_stack_cell_handle#}

{#pointer *symbol_stack_cell as SymbolStackCellHandlePtr -> SymbolStackCellHandle #}

data SymbolStackCell = SymbolStackCell { head :: ScopedSymbol, tail :: SymbolStackCellHandle }

instance Storable SymbolStackCell where
  sizeOf _ = {#sizeof symbol_stack_cell#}
  alignment _ = {#alignof symbol_stack_cell#}
  peek p = do
    bod <- peek (castPtr p)
    len <- {#get symbol_stack_cell->tail #} p
    pure (SymbolStackCell bod len)
  poke p (SymbolStackCell bod len) = do
    poke (castPtr p) bod
    {#set symbol_stack_cell.tail #} p len

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

data SymbolStack = SymbolStack { cells :: SymbolStackCellHandlePtr, length :: CSize }

{#pointer *symbol_stack as SymbolStackPtr -> SymbolStack #}

instance Storable SymbolStack where
  sizeOf _ = {#sizeof symbol_stack#}
  alignment _ = {#alignof symbol_stack#}
  peek p = do
    bod <- peek (castPtr p)
    len <- {#get struct symbol_stack->length #} p
    pure (SymbolStack bod (fromIntegral len))
  poke p (SymbolStack bod len) = do
    poke (castPtr p) bod
    {#set struct symbol_stack.length #} p (fromIntegral len)

data ScopeStackCell = ScopeStackCell { head :: NodeHandle, tail :: ScopeStackCellHandle }

instance Storable ScopeStackCell where
  sizeOf _ = {#sizeof scope_stack_cell#}
  alignment _ = {#alignof scope_stack_cell#}
  peek p = do
    bod <- {#get scope_stack_cell->head #} p
    len <- {#get scope_stack_cell->tail #} p
    pure (ScopeStackCell (fromIntegral bod) (fromIntegral len))
  poke p (ScopeStackCell bod len) = do
    {#set scope_stack_cell.head #} p (fromIntegral bod)
    {#set scope_stack_cell.tail #} p (fromIntegral len)


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

instance Storable PathEdge where
  sizeOf _ = {#sizeof path_edge #}
  alignment _ = {#alignof path_edge #}
  peek p = do
    node <- peek (castPtr p)
    prec <- {#get path_edge->precedence #} p
    pure (PathEdge node (fromIntegral prec))
  poke p (PathEdge node prec) = do
    poke (castPtr p) node
    {#set path_edge.precedence #} p (fromIntegral prec)

type PathEdgeListCellHandle = {#type path_edge_list_cell_handle#}

data PathEdgeListCell = PathEdgeListCell
  { head :: PathEdge
  , tail :: PathEdgeListCellHandle
  , reversed :: PathEdgeListCellHandle
  }

instance Storable PathEdgeListCell where
  sizeOf _ = {#sizeof path_edge_list_cell #}
  alignment _ = {#alignof path_edge_list_cell #}
  peek p = do
    bod <- peek (castPtr p)
    dir <- {#get path_edge_list_cell->tail #} p
    len <- {#get path_edge_list_cell->reversed #} p
    pure (PathEdgeListCell bod dir len)
  poke p (PathEdgeListCell bod dir len) = do
    poke (castPtr p) bod
    {#set path_edge_list_cell.tail #} p (fromIntegral dir)
    {#set path_edge_list_cell.reversed #} p (fromIntegral len)

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

instance Storable PathEdgeList where
  sizeOf _ = {#sizeof path_edge_list #}
  alignment _ = {#alignof path_edge_list #}
  peek p = do
    bod <- {#get path_edge_list->cells #} p
    dir <- {#get path_edge_list->direction #} p
    len <- {#get path_edge_list->length #} p
    pure (PathEdgeList bod (toEnum (fromIntegral dir)) (fromIntegral len))
  poke p (PathEdgeList bod dir len) = do
    {#set path_edge_list.cells #} p bod
    {#set path_edge_list.direction #} p (fromIntegral (fromEnum dir))
    {#set path_edge_list.length #} p (fromIntegral len)

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

sizeOf' :: forall a . Storable a => Int
sizeOf' = sizeOf (undefined :: a)

advance :: forall t a . Storable t => Ptr t -> Ptr a
advance p = castPtr p `plusPtr` (sizeOf' @t)

instance Storable Path where
  sizeOf _ = {#sizeof path#}
  alignment _ = {#alignof path#}
  peek p = do
    sn <- {#get path->start_node #} p
    en <- {#get path->end_node #} p
    let p1 = castPtr p `plusPtr` (sizeOf' @NodeHandle * 2)
    sym <- peek p1
    let p2 = advance @SymbolStack p1
    sco <- peek p2
    let p3 = advance @ScopeStack p2
    edg <- peek p3
    pure (Path sn en sym sco edg)
  poke p (Path sn en sym sco edg) = do
    {#set path->start_node#} p sn
    {#set path->end_node#} p en
    let p1 = castPtr p `plusPtr` (sizeOf' @NodeHandle * 2)
    poke p1 sym
    let p2 = advance @SymbolStack p1
    poke p2 sco
    let p3 = advance @ScopeStack p2
    poke p3 edg

type PartialScopeStackCellHandle = {#type partial_scope_stack_cell_handle#}

type ScopeStackVariable = {#type scope_stack_variable#}

data PartialScopeStack = PartialScopeStack
  { cells :: PartialScopeStackCellHandle
  , direction :: DequeDirection
  , variable :: ScopeStackVariable
  }

instance Storable PartialScopeStack where
  sizeOf _ = {#sizeof partial_scope_stack#}
  alignment _ = {#alignof partial_scope_stack#}
  peek p = do
    hdl <- {#get partial_scope_stack->cells #} p
    dir <- {#get partial_scope_stack->direction #} p
    var <- {#get partial_scope_stack->variable #} p
    pure (PartialScopeStack hdl (toEnum (fromIntegral dir)) var)
  poke p (PartialScopeStack hdl dir var) = do
    {#set partial_scope_stack->cells #} p hdl
    {#set partial_scope_stack->direction #} p (fromIntegral (fromEnum dir))
    {#set partial_scope_stack->variable #} p var

data PartialScopedSymbol = PartialScopedSymbol { symbol :: SymbolHandle, scopes :: PartialScopeStack }

instance Storable PartialScopedSymbol where
  sizeOf _ = {#sizeof partial_scoped_symbol #}
  alignment _ = {#alignof partial_scoped_symbol #}
  peek p = do
    sym <- {#get partial_scoped_symbol->symbol #} p
    sco <- peek (castPtr p `plusPtr` sizeOf (undefined :: SymbolHandle))
    pure (PartialScopedSymbol sym sco)
  poke p (PartialScopedSymbol sym sco) = do
    {#set partial_scoped_symbol.symbol #} p (fromIntegral sym)
    poke (castPtr p `plusPtr` sizeOf (undefined :: SymbolHandle)) sco

type PartialSymbolStackCellHandle = {#type partial_symbol_stack_cell_handle#}

data PartialSymbolStackCell = PartialSymbolStackCell
  { head :: PartialScopedSymbol
  , tail :: PartialSymbolStackCellHandle
  , reversed :: PartialSymbolStackCellHandle
  }

instance Storable PartialSymbolStackCell where
  sizeOf _ = {#sizeof partial_symbol_stack_cell #}
  alignment _ = {#alignof partial_symbol_stack_cell #}
  peek p = do
    bod <- peek (castPtr p)
    dir <- {#get partial_symbol_stack_cell->tail #} p
    len <- {#get partial_symbol_stack_cell->reversed #} p
    pure (PartialSymbolStackCell bod dir len)
  poke p (PartialSymbolStackCell bod dir len) = do
    poke (castPtr p) bod
    {#set partial_symbol_stack_cell.tail #} p (fromIntegral dir)
    {#set partial_symbol_stack_cell.reversed #} p (fromIntegral len)

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

instance Storable PartialSymbolStack where
  sizeOf _ = {#sizeof partial_symbol_stack #}
  alignment _ = {#alignof partial_symbol_stack #}
  peek p = do
    bod <- {#get partial_symbol_stack->cells #} p
    dir <- {#get partial_symbol_stack->direction #} p
    pure (PartialSymbolStack (fromIntegral bod) (toEnum (fromIntegral dir)))
  poke p (PartialSymbolStack bod dir) = do
    {#set partial_symbol_stack.cells #} p bod
    {#set partial_symbol_stack.direction #} p (fromIntegral (fromEnum dir))

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

instance Storable PartialPathEdgeList where
  sizeOf _ = {#sizeof partial_path_edge_list #}
  alignment _ = {#alignof partial_path_edge_list #}
  peek p = do
    bod <- {#get partial_path_edge_list->cells #} p
    dir <- {#get partial_path_edge_list->direction #} p
    len <- {#get partial_path_edge_list->length #} p
    pure (PartialPathEdgeList bod (toEnum (fromIntegral dir)) (fromIntegral len))
  poke p (PartialPathEdgeList bod dir len) = do
    {#set partial_path_edge_list.cells #} p bod
    {#set partial_path_edge_list.direction #} p (fromIntegral (fromEnum dir))
    {#set partial_path_edge_list.length #} p (fromIntegral len)

data PartialPath = PartialPath
  { startNode :: NodeHandle
  , endNode :: NodeHandle
  , symbolStackPrecondition :: PartialSymbolStack
  , symbolStackPostcondition :: PartialSymbolStack
  , scopeStackPrecondition :: PartialScopeStack
  , scopeStackPostcondition :: PartialScopeStack
  , edges :: PartialPathEdgeList
  }

instance Storable PartialPath where
  sizeOf _ = {#sizeof partial_path#}
  alignment _ = {#sizeof partial_path#}
  peek p = do
    sn <- {#get partial_path->start_node #} p
    en <- {#get partial_path->end_node #} p
    sympre <- peekByteOff (castPtr p) {#offsetof partial_path->symbol_stack_precondition#}
    sympos <- peekByteOff (castPtr p) {#offsetof partial_path->symbol_stack_postcondition#}
    scopre <- peekByteOff (castPtr p) {#offsetof partial_path->scope_stack_precondition#}
    scopos <- peekByteOff (castPtr p) {#offsetof partial_path->scope_stack_postcondition#}
    edg <- peekByteOff (castPtr p) {#offsetof partial_path->edges#}
    pure (PartialPath sn en sympre sympos scopre scopos edg)
  poke p (PartialPath sn en sympre sympos scopre scopos edg) = do
    {#set partial_path->start_node #} p sn
    {#set partial_path->end_node #} p en
    pokeByteOff (castPtr p) {#offsetof partial_path->symbol_stack_precondition#} sympre
    pokeByteOff (castPtr p) {#offsetof partial_path->symbol_stack_postcondition#} sympos
    pokeByteOff (castPtr p) {#offsetof partial_path->scope_stack_precondition#} scopre
    pokeByteOff (castPtr p) {#offsetof partial_path->scope_stack_postcondition#} scopos
    pokeByteOff (castPtr p) {#offsetof partial_path->edges#} edg

{#pointer *partial_path as PartialPathPtr -> PartialPath #}

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
    `NodePtr',
    `NodeHandlePtr'} -> `()' #}

{#fun unsafe stack_graph_add_edges as ^
  { id `StackGraphPtr', fromIntegral `CSize', castPtr `Ptr Node'} -> `()' #}

{#fun unsafe path_arena_symbol_stack_cells as ^ { castPtr `PathArenaPtr'} -> `SymbolStackCells' castPeek* #}

{#fun unsafe path_arena_add_symbol_stacks as ^
  { id `PathArenaPtr',
    fromIntegral `CSize',
    castPtr `Ptr ScopedSymbol',
    castPtr `Ptr CSize',
    `SymbolStackPtr'} -> `()' #}

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

-- {#fun unsafe partial_path_arena_add_partial_symbol_stacks as ^
--   { id `PartialPathArenaPtr',
--     fromIntegral `CSize',
--     castPtr `Ptr PartialScopedSymbol',
--     castPtr `Ptr CSize',
--     castPtr `Ptr PartialSymbolStack'} -> `()' #}

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
    castPtr `Ptr PartialPathEdge',
    castPtr `Ptr CSize',
    castPtr `Ptr PartialPathEdgeList'} -> `()' #}

{#fun unsafe partial_path_list_new as ^ {} -> `PartialPathListPtr' #}
{#fun unsafe partial_path_list_free as ^ {`PartialPathListPtr'} -> `()' #}

{#fun partial_path_list_count as ^
  { `PartialPathListPtr' } -> `CSize' fromIntegral #}

{#fun partial_path_list_paths as ^ { `PartialPathListPtr' } -> `PartialPathPtr' #}

{#fun unsafe partial_path_arena_find_partial_paths_in_file as ^
  { `StackGraphPtr',
    `PartialPathArenaPtr',
    fromIntegral `FileHandle',
    `PartialPathListPtr' } -> `()' #}

{#fun partial_path_database_add_partial_paths as ^
  { `StackGraphPtr',
    `PartialPathArenaPtr',
    `PartialPathDatabasePtr',
    fromIntegral `CSize',
    `PartialPathPtr',
    castPtr `Ptr PartialPathHandle' } -> `()' #}

-- TODO stitcher
