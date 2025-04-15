#ifndef MEMORY_OPERATIONS_H
#define MEMORY_OPERATIONS_H

#include "libraries.h"
#include "data_structures.h"

void create_database_file();
void open_database_file(int* db_filedescriptor);
void get_file_size(DBFile* db);
void memory_map_file(DBFile* db);
void set_file_dirty(DBFile* db, bool dirty);
void set_new_file_free_blocks(DBFile* db);
void init_create_memory_block(DBFile* db);
void create_memory_block(DBFile* db);
void write_on_memory_block(DBFile *db, const void* new_data, u_int64_t page_num);
void commit_changes(DBFile *db);

#endif