#ifndef HS_STACK_GRAPHS
#define HS_STACK_GRAPHS

#include <string.h>
#include "stack-graphs.h"

void sg_stack_graph_symbols_ptr(struct sg_stack_graph *graph, struct sg_symbols *syms);
void sg_stack_graph_files_ptr(struct sg_stack_graph *graph, struct sg_files *files);

void sg_partial_path_arena_partial_path_edge_list_cells_ptr(struct sg_partial_path_arena *graph, struct sg_partial_path_edge_list_cells *syms);

#endif // HS_STACKGRAPHS
