#ifndef STATEMENT_PARSE_H
#define STATEMENT_PARSE_H

#include "../libraries.h"
#include "../data/parser_structures.h"
#include "../utils/cJSON.h"

int parse_statement(const char *filename, Statement *stmt) ;

#endif 
