#ifndef GLOBALS_H
#define GLOBALS_H

#include "../data/core_structures.h"

extern RowNode *root;
extern Queue* free_page_queue;
extern NodeEntry visited_nodes[MAX_VISITED_NODES];
extern int visited_count;
extern uint64_t serialized_pages[MAX_VISITED_NODES];
extern int serialized_count;
extern DBFile* db;
extern DataFile* df;
extern MetadataPage* metadata;
extern uint64_t global_id;
extern int hist_len;
extern struct termios orig_termios;
extern char *history[MAX_HISTORY];


#endif