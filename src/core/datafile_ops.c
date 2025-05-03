#include "../../include/core/datafile_ops.h"
#include "../../include/data/parser_structures.h"
#include "../../include/utils/globals.h"

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

bool serialize_datafile(DataFile* df) {
    if (!df) {
        fprintf(stderr, "Error: NULL DataFile pointer\n");
        return false;
    }

    if (!df->start_ptr) {
        fprintf(stderr, "Error: NULL start_ptr in DataFile\n");
        return false;
    }

    if (!df->write_ptr) {
        fprintf(stderr, "Error: NULL write_ptr in DataFile\n");
        return false;
    }

    typedef struct {
        uint64_t write_ptr_offset;
        uint64_t magic;
    } DataFileHeader;

    DataFileHeader header;
    header.write_ptr_offset = (char*)df->write_ptr - (char*)df->start_ptr;
    header.magic = MAGIC_NUMBER;

    if (header.write_ptr_offset >= df->size) {
        fprintf(stderr, "Error: Invalid write_ptr offset: %lu exceeds file size: %ld\n",
                header.write_ptr_offset, df->size);
        return false;
    }

    if (memcpy(df->start_ptr, &header, sizeof(header)) != df->start_ptr) {
        fprintf(stderr, "Error: Failed to write header to file\n");
        return false;
    }

    if (msync(df->start_ptr, sizeof(header), MS_SYNC) == -1) {
        perror("Error syncing header to disk");
        return false;
    }

    return true;
}

bool deserialize_datafile(DataFile* df) {
    if (!df) {
        fprintf(stderr, "Error: NULL DataFile pointer\n");
        return false;
    }

    if (!df->start_ptr) {
        fprintf(stderr, "Error: NULL start_ptr in DataFile\n");
        return false;
    }

    if (df->size < sizeof(uint64_t) * 2) {
        fprintf(stderr, "Error: File too small to contain header\n");
        return false;
    }

    typedef struct {
        uint64_t write_ptr_offset;
        uint64_t magic;
    } DataFileHeader;


    DataFileHeader header;
    memcpy(&header, df->start_ptr, sizeof(header));

    if (header.magic != MAGIC_NUMBER) {
        fprintf(stderr, "Error: Invalid file format (magic number mismatch)\n");
        return false;
    }

    if (header.write_ptr_offset >= df->size) {
        fprintf(stderr, "Error: Invalid write_ptr offset in file: %lu\n",
                header.write_ptr_offset);
        return false;
    }

    df->write_ptr = (char*)df->start_ptr + header.write_ptr_offset;

    return true;
}

void set_file_dirty_df(DataFile* df,bool dirty) {
    df->dirty= dirty;
}

void commit_changes_df(DataFile *df) {
    if (df->dirty) {
        if (!serialize_datafile(df)) {
            fprintf(stderr, "Error: Failed to serialize data file\n");
            return;
        }

        set_file_dirty_df(df, false);
        printf("Changes (raw data) successfully committed to disk.\n");
    }
}

void load_datafile(DataFile* df) {
    if (!deserialize_datafile(df)) {
        fprintf(stderr, "Error: Failed to deserialize data file\n");
        return;
    }
    printf("Data file loaded successfully.\n");
    printf("Write pointer offset: %p\n", df->write_ptr - df->start_ptr);
}

int string_to_int(const char* str) {
    if (str == NULL) {
        return 0;
    }

    int result = 0;
    int sign = 1;
    int i = 0;

    if (str[0] == '-') {
        sign = -1;
        i = 1;
    } else if (str[0] == '+') {
        i = 1;
    }

    while (str[i] != '\0') {
        if (str[i] >= '0' && str[i] <= '9') {
            if (result > INT_MAX / 10 ||
                (result == INT_MAX / 10 && str[i] - '0' > INT_MAX % 10)) {
                return (sign == 1) ? INT_MAX : INT_MIN;
                }

            result = result * 10 + (str[i] - '0');
        } else {
            break;
        }
        i++;
    }

    return result * sign;
}


void serialize_int(Statement *stmt,void*row_content,uint64_t *row_index,uint64_t index) {
    int64_t value = string_to_int(stmt->insertStmt.values[index].value);
    void* row_content_int = malloc(sizeof(int64_t));

    *(int64_t*)row_content_int = value;
    memcpy(row_content + (*row_index), row_content_int, sizeof(int64_t));

    *row_index += sizeof(int64_t);

    free(row_content_int);
}

void serialize_string(Statement *stmt,void*row_content,uint64_t *row_index,uint64_t index) {
    uint32_t string_length = strlen(stmt->insertStmt.values[index].value); // lenght in bytes of str lenght is 4 bytes

    void* string_lenght_int = malloc(sizeof(uint32_t));
    *(uint32_t*)string_lenght_int = string_length;
    memcpy(row_content + (*row_index), string_lenght_int, sizeof(uint32_t));

    *row_index += sizeof(uint32_t);

    void* string_content = malloc(string_length);
    memcpy(string_content, stmt->insertStmt.values[index].value, string_length);
    memcpy(row_content + (*row_index), string_content, string_length);

    *row_index += string_length;

    free(string_lenght_int);
    free(string_content);
}

void set_row_flag(void* row_content,bool flag) {
    memccpy(row_content+8 , &flag, sizeof(bool), 1);
}

void set_row_size(void* row_content,uint64_t size) {
    memcpy(row_content, &size, sizeof(uint64_t));
}


void* get_row_content(Statement *stmt, uint64_t *row_index) {
    void *row_content = malloc(MAX_BUFFER_SIZE);

    set_row_flag(row_content,true);

    uint64_t index = 0;

    *row_index += 9;

    while (stmt->insertStmt.values[index].value != NULL) {
        printf("%ld\n", index);
        if (strcmp(stmt->insertStmt.values[index].valueType,"Int") == 0) {
            serialize_int(stmt,row_content,row_index,index);
        }
        else if (strcmp(stmt->insertStmt.values[index].valueType,"String") == 0) {
            // printf("%ld\n",strlen(stmt->insertStmt.values[index].value));
            serialize_string(stmt,row_content,row_index,index);
        }
        index+=1;
    }

    set_row_size(row_content,*row_index);

    return row_content;
}

void print_entire_table(RowNode *node, DataFile *df) {
    if (node == NULL) {
        printf("Node is NULL\n");
        return;
    }

    for (int i=0; i<node->num_keys; i+=1) {
        uint64_t offset = node->raw_data[i] - df->start_ptr;
        printf("Offset: %ld\n", offset);
    }
}