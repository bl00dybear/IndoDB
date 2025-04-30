#include "../headers/datafile_ops.h"

void* write_row(DataFile* df, void *row, size_t row_size) {
    void* written_address = df->write_ptr;
    if ((char*)df->write_ptr - (char*)df->write_ptr + row_size > df->size) {
        // Aici ar trebui să extinzi fișierul
        // TODO: implement file extension
        return;
    }

    memcpy(df->write_ptr, row, row_size);

    df->write_ptr = (char*)df->write_ptr + row_size;

    df->dirty = true;

    return written_address;
}