#ifndef STATEMENT_OPS_H
#define STATEMENT_OPS_H

#include <dirent.h>
#include "../data/parser_structures.h"
#include "../data/core_structures.h"
#include "../utils/globals.h"
#include "../core/database_ops.h"
#include "../core/row_btree_ops.h"

void process_statement(Statement *stmt);
void free_statement(Statement *stmt);
bool verify_constraints(Statement *stmt, MetadataPage *metadata);


#endif 
