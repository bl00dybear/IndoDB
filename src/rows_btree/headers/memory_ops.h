#ifndef MEMORY_OPERATIONS_H
#define MEMORY_OPERATIONS_H

#include "libraries.h"
#include "data_structures.h"

void create_database_file(char file_name[]);
void open_database_file(int* db_filedescriptor,char file_name[]);
void get_db_file_size(DBFile* db);
void get_df_file_size(DataFile* db);
void memory_map_db_file(DBFile* db);
void memory_map_df_file(DataFile* df);
void set_file_dirty(DBFile* db, bool dirty);
void set_new_file_free_blocks(DBFile* db);
void init_create_db_memory_block(DBFile* db);
void init_create_df_memory_block(DataFile* df);
void create_memory_block(DBFile* db);
void write_on_memory_block(DBFile *db, const void* new_data, u_int64_t page_num);
void commit_changes(DBFile *db);

#endif