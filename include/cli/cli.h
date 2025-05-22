#ifndef CLI_H
#define CLI_H

#include <ctype.h>
#include "../libraries.h"
#include "../utils/globals.h"
#include "../data/parser_structures.h"
#include "statement_parse.h"
#include "statement_ops.h"

int cli();
int free_memory(Statement* stmt);

#endif 
