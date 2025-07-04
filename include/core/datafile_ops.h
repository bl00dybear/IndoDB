#ifndef DATAFILE_OPS_C
#define DATAFILE_OPS_C

#include "../data/core_structures.h"
#include "../data/parser_structures.h"
#include "../core/row_btree_ops.h"

uint64_t write_row(DataFile* df, const void *row, size_t row_size);
void commit_changes_df(DataFile* df);
void set_file_dirty_df(DataFile*df,bool dirty);
void load_datafile(DataFile* df);
void* get_row_content(Statement *stmt, uint64_t * row_index);
// void print_entire_table(RowNode *node, DataFile *df, MetadataPage *metadatam,Statement *stmt);

void set_table_parameters(MetadataPage *metadata, Statement *stmt);
void display_table(char **columns, int num_columns, MetadataPage *meta, Statement *stmt);

bool constraint_unique(RowNode *node, Statement*stmt, MetadataPage *metadata,int column_index);

void delete_rows(Statement *stmt, MetadataPage *metadata,int num_columns);
void update_rows(Statement *stmt, MetadataPage *metadata,int num_columns);


#endif