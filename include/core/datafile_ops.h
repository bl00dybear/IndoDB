#ifndef DATAFILE_OPS_C
#define DATAFILE_OPS_C

#include "../data/core_structures.h"

void* write_row(DataFile* df, const void *row, size_t row_size);
void commit_changes_df(DataFile* df);
void set_file_dirty_df(DataFile*df,bool dirty);
void load_datafile(DataFile* df);

#endif