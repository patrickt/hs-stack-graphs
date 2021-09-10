
#include <string.h>
#include <stdio.h>
#include "hs_stack_graphs.h"

void sg_stack_graph_symbols_ptr(struct sg_stack_graph *graph, struct sg_symbols *syms) {
  struct sg_symbols items = sg_stack_graph_symbols(graph);
  syms->symbols = items.symbols;
  syms->count = items.count;
}

void sg_partial_path_arena_partial_path_edge_list_cells_ptr(struct sg_partial_path_arena *arena, struct sg_partial_path_edge_list_cells *syms) {
  struct sg_partial_path_edge_list_cells items = sg_partial_path_arena_partial_path_edge_list_cells(arena);
  memcpy(syms, &items, sizeof items);
}
