
#include <string.h>
#include "hs_stack_graphs.h"

struct sg_symbols *sg_stack_graph_symbols_new(struct sg_stack_graph *graph) {
  struct sg_symbols items = sg_stack_graph_symbols(graph);
  struct sg_symbols *dest = calloc(sizeof(struct sg_symbols), 1);
  memcpy(dest, &items, sizeof(struct sg_symbols));
  return dest;
}

void sg_stack_graph_symbols_ptr(struct sg_stack_graph *graph, struct sg_symbols *syms) {
  struct sg_symbols items = sg_stack_graph_symbols(graph);
  memcpy(syms, &items, sizeof items);
}

void sg_partial_path_arena_partial_path_edge_list_cells_ptr(struct sg_partial_path_arena *arena, struct sg_partial_path_edge_list_cells *syms) {
  struct sg_partial_path_edge_list_cells items = sg_partial_path_arena_partial_path_edge_list_cells(arena);
  memcpy(syms, &items, sizeof items);
}


size_t sg_symbols_count(struct sg_symbols *syms) {
  return syms->count;
}

struct sg_symbol *sg_symbols_contents(struct sg_symbols *syms) {
  return syms->symbols;
}

void sg_symbols_free(struct sg_symbols *syms) {
  free(syms);
}
