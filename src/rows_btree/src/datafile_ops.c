#include "../headers/datafile_ops.h"

void allocate_new_block(DataFile *df) {
    const size_t new_size = df->size + BLOCK_SIZE;

    if (ftruncate(df->fd, new_size) == -1) {
        perror("Error extending data file size");
        exit(EXIT_FAILURE);
    }

    if (munmap(df->write_ptr, df->size) == -1) {
        perror("Error unmapping old data file");
        exit(EXIT_FAILURE);
    }

    const void* mapped_region = mmap(NULL, new_size, PROT_READ | PROT_WRITE, MAP_SHARED, df->fd, 0);
    if (mapped_region == MAP_FAILED) {
        perror("Error mapping expanded data file");
        exit(EXIT_FAILURE);
    }

    // Setăm write_ptr la începutul noului bloc
    // df->write_ptr = (char*)mapped_region + df->size;  // df->size este vechea dimensiune
    df->size = new_size;
    df->dirty = true;

    printf("\nData file extended to size: %zu bytes\n", new_size);
    printf("New block starts at offset: %ld\n", df->size - BLOCK_SIZE);
}


void* write_row(DataFile* df, const void *row, const size_t row_size) {

    printf("\nFile size: %ld\n",df->size);
    printf("Row size: %ld\n",row_size);

    if (df->size == PAGE_SIZE) {
        allocate_new_block(df);
    }

    printf("\nFile size: %ld\n",df->size);


    const void* start_ptr = mmap(NULL, df->size, PROT_READ | PROT_WRITE, MAP_SHARED, df->fd, 0);
    if (start_ptr == MAP_FAILED) {
        perror("mmap failed");
        return NULL;
    }

    printf("\nStart ptr: %p\n", start_ptr);
    printf("Write ptr: %p\n", df->write_ptr);

    size_t current_position = (size_t)(df->write_ptr-start_ptr)-BLOCK_SIZE*2;

    printf("\nCurrent position: %p\n", current_position);

    while (current_position + row_size > df->size) {
        printf("\nCurrent pos + row size %p \n",current_position + row_size);
        printf("Data file size %ld \n",df->size);
        allocate_new_block(df);
    }


    void* written_address = df->write_ptr;
    // if ((char*)df->write_ptr - (char*)df->write_ptr + row_size > df->size) {
    //     // Aici ar trebui să extinzi fișierul
    //     // TODO: implement file extension
    //     return;
    // }

    memcpy(df->write_ptr, row, row_size);

    df->write_ptr = (char*)df->write_ptr + row_size;

    df->dirty = true;

    return written_address;
}