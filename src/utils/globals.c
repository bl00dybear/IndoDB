#include "../../include/utils/globals.h"
#include "../../include/data/core_structures.h"

RowNode *root = NULL;
Queue* free_page_queue = NULL;
NodeEntry visited_nodes[MAX_VISITED_NODES];
int visited_count = 0;
uint64_t serialized_pages[MAX_VISITED_NODES];
int serialized_count = 0;
DBFile* db;
DataFile* df;
uint64_t global_id = 1;