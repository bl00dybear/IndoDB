#include "../headers/data_structures.h"

struct RowNode *root = NULL;
Queue* free_page_queue = NULL;
NodeEntry visited_nodes[MAX_VISITED_NODES];
int visited_count = 0;
uint64_t serialized_pages[MAX_VISITED_NODES];
int serialized_count = 0;