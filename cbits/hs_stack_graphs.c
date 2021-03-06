
#include <string.h>
#include <stdio.h>
#include "hs_stack_graphs.h"

inline void sg_stack_graph_symbols_ptr(struct sg_stack_graph *graph, struct sg_symbols *syms) {
  struct sg_symbols items = sg_stack_graph_symbols(graph);
  syms->symbols = items.symbols;
  syms->count = items.count;
}

inline void sg_stack_graph_files_ptr(struct sg_stack_graph *graph, struct sg_files *files)
{
  struct sg_files items = sg_stack_graph_files(graph);
  files->files = items.files;
  files->count = items.count;
}

inline void sg_partial_path_arena_partial_path_edge_list_cells_ptr(struct sg_partial_path_arena *arena, struct sg_partial_path_edge_list_cells *syms) {
  struct sg_partial_path_edge_list_cells items = sg_partial_path_arena_partial_path_edge_list_cells(arena);
  memcpy(syms, &items, sizeof items);
}

inline void sg_stack_graph_nodes_ptr(struct sg_stack_graph *graph, struct sg_nodes *nodes)
{
  struct sg_nodes items = sg_stack_graph_nodes(graph);
  nodes->nodes = items.nodes;
  nodes->count = items.count;
}
