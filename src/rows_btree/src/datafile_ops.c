#include "../headers/datafile_ops.h"

void allocate_new_block(DataFile *df) {
    const size_t new_size = df->size + BLOCK_SIZE;
    const size_t current_offset = (char*)df->write_ptr - (char*)df->start_ptr;

    void* temp_buffer = NULL;
    if (df->size > 0) {
        temp_buffer = malloc(df->size);
        if (temp_buffer == NULL) {
            perror("Error allocating temporary buffer");
            exit(EXIT_FAILURE);
        }
        memcpy(temp_buffer, df->start_ptr, df->size);
    }

    if (ftruncate(df->fd, new_size) == -1) {
        perror("Error extending data file size");
        exit(EXIT_FAILURE);
    }

    if (munmap(df->start_ptr, df->size) == -1) {
        perror("Error unmapping old data file");
        exit(EXIT_FAILURE);
    }

    void* new_mapping = mmap(NULL, new_size, PROT_READ | PROT_WRITE, MAP_SHARED, df->fd, 0);
    if (new_mapping == MAP_FAILED) {
        perror("Error mapping expanded data file");
        exit(EXIT_FAILURE);
    }

    if (temp_buffer != NULL) {
        memcpy(new_mapping, temp_buffer, df->size);
        free(temp_buffer);
    }

    df->start_ptr = new_mapping;
    df->write_ptr = (char*)new_mapping + current_offset;
    df->size = new_size;
    df->dirty = true;

    printf("\nData file extended to size: %zu bytes\n", new_size);
    printf("New block starts at offset: %ld\n", df->size - BLOCK_SIZE);
}

void* write_row(DataFile* df, const void *row, const size_t row_size) {
    printf("\nFile size: %ld\n", df->size);
    printf("Row size: %ld\n", row_size);

    size_t current_position = (size_t)(df->write_ptr - df->start_ptr);


    while (current_position + (size_t)row_size > (size_t)df->size) {
        allocate_new_block(df);
        current_position = (size_t)(df->write_ptr - df->start_ptr);
    }

    void* written_address = df->write_ptr;
    memcpy(df->write_ptr, row, row_size);
    df->write_ptr = (char*)df->write_ptr + row_size;
    df->dirty = true;

    return written_address;
}