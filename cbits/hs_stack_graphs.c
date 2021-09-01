
#include <string.h>
#include "hs_stack_graphs.h"

void sg_stack_graph_symbols_ptr(struct sg_stack_graph *graph, struct sg_symbols *syms) {
  struct sg_symbols items = sg_stack_graph_symbols(graph);
  memcpy(syms, &items, sizeof items);
}
