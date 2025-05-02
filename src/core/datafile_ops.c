#include "../../include/core/datafile_ops.h"
#include "../../include/data/parser_structures.h"

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
    // Verificare dacă șirul este NULL
    if (str == NULL) {
        return 0;
    }

    int result = 0;
    int sign = 1;
    int i = 0;

    // Verificare semn
    if (str[0] == '-') {
        sign = -1;
        i = 1; // Începe de la următorul caracter
    } else if (str[0] == '+') {
        i = 1; // Începe de la următorul caracter
    }

    // Parcurge șirul și construiește numărul
    while (str[i] != '\0') {
        // Verifică dacă caracterul este o cifră
        if (str[i] >= '0' && str[i] <= '9') {
            // Evită overflow
            if (result > INT_MAX / 10 ||
                (result == INT_MAX / 10 && str[i] - '0' > INT_MAX % 10)) {
                // Returnează INT_MAX sau INT_MIN în caz de overflow
                return (sign == 1) ? INT_MAX : INT_MIN;
                }

            // Adaugă cifra la rezultat
            result = result * 10 + (str[i] - '0');
        } else {
            // Dacă nu este cifră, oprește procesarea
            break;
        }
        i++;
    }

    return result * sign;
}

char* int_to_bytes_string(int val) {
    char* result = (char*)malloc(sizeof(char)*9);

    if (val == 0) {
        for (int i = 0; i < 8; i++) {
            result[i] = '\0';
        }
        result[8] = '\0';
    }
    for (int i = 0; i < 8; i++) {
        printf("%ld",result[i]);
    }
    // sprintf(result, "%d", val);
    return result;
}


char* get_row_content(Statement *stmt) {
    char *row_content = malloc(MAX_BUFFER_SIZE);
    size_t row_size = 0;
    size_t num_of_values = sizeof(stmt->insertStmt.values) * 4 / sizeof(stmt->insertStmt.values[0]);

    u_int64_t i = 0;

    while (stmt->insertStmt.values[i].value != NULL) {
        if (strcmp(stmt->insertStmt.values[i].valueType,"Int") == 0) {
            // printf("aia");
            int value = string_to_int(stmt->insertStmt.values[i].value);
            char* bytes_string = int_to_bytes_string(value);
        }
        else if (strcmp(stmt->insertStmt.values[i].valueType,"String") == 0) {

        }
        // printf("%s\n",stmt->insertStmt.values[i].value);
        i+=1;
    }
    // printf("%ld\n",sizeof(stmt->insertStmt.values));
    // printf("%ld\n",sizeof(stmt->insertStmt.values[0]));
    //
    // printf("%ld\n",num_of_values);

    return row_content;
}