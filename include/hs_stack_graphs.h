#ifndef HS_STACK_GRAPHS
#define HS_STACK_GRAPHS

#include <string.h>
#include "stack-graphs.h"

void sg_stack_graph_symbols_ptr(struct sg_stack_graph *graph, struct sg_symbols *syms);
void sg_partial_path_arena_partial_path_edge_list_cells_ptr(struct sg_partial_path_arena *graph, struct sg_partial_path_edge_list_cells *syms);

struct sg_symbols *sg_stack_graph_symbols_new(struct sg_stack_graph *graph);
void sg_symbols_free(struct sg_symbols *syms);
size_t sg_symbols_count(struct sg_symbols *syms);
struct sg_symbol *sg_symbols_contents(struct sg_symbols *syms);

#endif // HS_STACKGRAPHS
