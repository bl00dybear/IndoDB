#include "../../include/core/datafile_ops.h"
#include "../../include/data/parser_structures.h"
#include "../../include/utils/globals.h"
#include "../../include/data/core_structures.h"

void allocate_new_block(DataFile *df) {
    const size_t new_size = df->size + BLOCK_SIZE;
    const size_t current_offset = df->write_ptr;

    // printf("Just before allocating new block\n");
    // printf("Current offset: %ld\n", current_offset);

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
    df->write_ptr = current_offset;
    df->size = new_size;
    df->dirty = true;
    
    // printf("\nData file extended to size: %zu bytes\n", new_size);
    // printf("New block starts at offset: %ld\n", df->size - BLOCK_SIZE);
}

uint64_t write_row(DataFile* df, const void *row, const size_t row_size) {
    // printf("\nFile size: %ld\n", df->size);
    // printf("Row size: %ld\n", row_size);

    uint64_t current_position = df->write_ptr;


    // printf("Current position: %ld\n", current_position);

    while (current_position + (size_t)row_size > (size_t)df->size) {
        allocate_new_block(df);
        // printf("File size %ld\n", df->size);
        // printf("Row size %ld\n", current_position + (size_t)row_size);
        // current_position = (size_t)(df->write_ptr);
    }

    uint64_t written_address = df->write_ptr;
    memcpy(df->start_ptr+df->write_ptr, row, row_size);
    df->write_ptr = df->write_ptr + row_size;
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
        uint64_t magic;
        uint64_t write_ptr_offset;
        
    } DataFileHeader;

    DataFileHeader header;
    header.write_ptr_offset = df->write_ptr;
    header.magic = MAGIC_NUMBER;

    if (header.write_ptr_offset >= (uint64_t)df->size) {
        fprintf(stderr, "Error: Invalid write_ptr offset: %lu exceeds file size: %lu\n",
                header.write_ptr_offset, (uint64_t)df->size);
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

    if ((uint64_t)df->size < sizeof(uint64_t) * 2) {
        fprintf(stderr, "Error: File too small to contain header\n");
        return false;
    }

    typedef struct {
        uint64_t magic;
        uint64_t write_ptr_offset;
        
    } DataFileHeader;


    DataFileHeader header;
    memcpy(&header, df->start_ptr, sizeof(header));

    if (header.magic != MAGIC_NUMBER) {
        fprintf(stderr, "Error: Invalid file format (magic number mismatch)\n");
        return false;
    }

    if (header.write_ptr_offset >= (uint64_t)df->size) {
        fprintf(stderr, "Error: Invalid write_ptr offset in file: %lu\n",
                header.write_ptr_offset);
        return false;
    }

    df->write_ptr = header.write_ptr_offset;

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
        // printf("Changes (raw data) successfully committed to disk.\n");
    }
}

void load_datafile(DataFile* df) {
    if (!deserialize_datafile(df)) {
        fprintf(stderr, "Error: Failed to deserialize data file\n");
        return;
    }
}



void serialize_int(Statement *stmt,void*row_content,uint64_t *row_index,uint64_t index) {
    int64_t value = strtoll(stmt->insertStmt.values[index].value, NULL, 10);
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
    memcpy(row_content+8 , &flag, sizeof(bool));
}

void set_row_size(void* row_content,uint64_t size) {
    memcpy(row_content, &size, sizeof(uint64_t));
}


void* get_row_content(Statement *stmt, uint64_t *row_index) {
    void *row_content = malloc(MAX_BUFFER_SIZE);

    set_row_flag(row_content,true);

    *row_index += 9;

    // printf("\n Num of val %ld\n", stmt->insertStmt.num_values);

    for (uint64_t index = 0; index < (uint64_t)stmt->insertStmt.num_values; index+=1) {
        // printf("%ld\n", index);
        if (strcmp(stmt->insertStmt.values[index].valueType,"Int") == 0) {
            serialize_int(stmt,row_content,row_index,index);
        }
        else if (strcmp(stmt->insertStmt.values[index].valueType,"String") == 0) {
            // printf("%ld\n",strlen(stmt->insertStmt.values[index].value));
            serialize_string(stmt,row_content,row_index,index);
        }
    }

    set_row_size(row_content,*row_index);

    return row_content;
}


void print_row_content(void* row_content, MetadataPage *metadata, int* column_indexes, int num_columns) {
    // Verificare pentru pointeri NULL
    if (!row_content || !metadata || !column_indexes || num_columns <= 0) {
        fprintf(stderr, "Error: Invalid parameters passed to print_row_content\n");
        return;
    }

    // Citire size din header cu verificare
    uint64_t row_size;
    memcpy(&row_size, row_content, sizeof(uint64_t));
    
    // Verificare dimensiune validă
    if (row_size < 9 || row_size > MAX_BUFFER_SIZE) {
        fprintf(stderr, "Error: Invalid row size: %lu\n", row_size);
        return;
    }

    bool flag;
    memcpy(&flag, row_content + 8, sizeof(bool));

    // Alocă memoria cu verificare
    void* row_content_mem = malloc(row_size-9);
    if (row_content_mem == NULL) {
        fprintf(stderr, "Error: Memory allocation failed in print_row_content\n");
        return;
    }
    
    memcpy(row_content_mem, row_content+9, row_size-9);

    uint64_t row_byte_index = 0;
    
    typedef struct {
        int type;
        union {
            char* str_value;
            int64_t int_value;
        };
        uint32_t str_len;
    } ColumnValue;
    
    // Citim toate valorile din rând
    ColumnValue values[MAX_COLUMNS];
    
    // Inițializare pentru eliberare de memorie în caz de eroare
    for (uint64_t col = 0; col < metadata->num_columns; col+=1) {
        values[col].type = -1;
        values[col].str_value = NULL;
    }
    
    // Citire date cu verificări de limitare
    for (uint64_t col = 0; col < metadata->num_columns; col+=1) {
        values[col].type = metadata->column_types[col];
        
        // Verificare depășire buffer
        if (row_byte_index >= row_size-9) {
            fprintf(stderr, "Error: Reached end of row data prematurely\n");
            goto cleanup;
        }
        
        switch (values[col].type) {
            case TYPE_VARCHAR: {
                // Verificare spațiu suficient pentru string length
                if (row_byte_index + sizeof(uint32_t) > row_size-9) {
                    fprintf(stderr, "Error: Not enough space for string length\n");
                    goto cleanup;
                }
                
                uint32_t string_len;
                memcpy(&string_len, row_content_mem + row_byte_index, sizeof(uint32_t));
                row_byte_index += sizeof(uint32_t);
                
                // Verificare lungime validă string
                if (string_len > MAX_BUFFER_SIZE || 
                    row_byte_index + string_len > row_size-9) {
                    fprintf(stderr, "Error: Invalid string length or insufficient data\n");
                    goto cleanup;
                }

                values[col].str_value = malloc(string_len + 1);
                if (values[col].str_value == NULL) {
                    fprintf(stderr, "Error: Failed to allocate memory for string\n");
                    goto cleanup;
                }
                
                memcpy(values[col].str_value, row_content_mem + row_byte_index, string_len);
                values[col].str_value[string_len] = '\0';
                values[col].str_len = string_len;
                row_byte_index += string_len;
                break;
            }
            case TYPE_INT: {
                // Verificare spațiu suficient pentru int
                if (row_byte_index + sizeof(int64_t) > row_size-9) {
                    fprintf(stderr, "Error: Not enough space for integer value\n");
                    goto cleanup;
                }
                
                memcpy(&values[col].int_value, row_content_mem + row_byte_index, sizeof(int64_t));
                row_byte_index += sizeof(int64_t);
                break;
            }
            default:
                values[col].type = -1;
                break;
        }
    }
    
    // Determinăm numărul de linii necesar pentru afișare cu verificarea indicilor coloanelor
    int max_lines = 1;
    for (int i = 0; i < num_columns; i+=1) {
        uint64_t col = column_indexes[i];
        
        // Verificare index valid
        if (col >= metadata->num_columns) {
            fprintf(stderr, "Warning: Column index %lu out of bounds\n", col);
            continue;
        }
        
        if (values[col].type == TYPE_VARCHAR && values[col].str_len > 0) {
            int lines_needed = (values[col].str_len + 19) / 20; // Rotunjire în sus
            if (lines_needed > max_lines) {
                max_lines = lines_needed;
            }
        }
    }
    
    // Afișăm valorile pe mai multe linii
    int column_width = 20;
    for (int line = 0; line < max_lines; line+=1) {
        if (line > 0) {
            printf("\n|"); // Începem o linie nouă
        }
        
        for (int i = 0; i < num_columns; i+=1) {
            uint64_t col = column_indexes[i];
            
            // Verificare index valid
            if (col >= metadata->num_columns) {
                printf(" %-*s |", column_width, "INVALID");
                continue;
            }
            
            switch (values[col].type) {
                case TYPE_VARCHAR: {
                    if (values[col].str_value == NULL) {
                        printf(" %-*s |", column_width, "NULL");
                        break;
                    }
                    
                    uint32_t start = line * 20;
                    if (start < values[col].str_len) {
                        uint32_t display_len = ((values[col].str_len - start) > 20) ? 20 : (values[col].str_len - start);
                        printf(" %-*.*s |", column_width, display_len, values[col].str_value + start);
                    } else {
                        printf(" %-*s |", column_width, ""); // Spațiu gol
                    }
                    break;
                }
                case TYPE_INT: {
                    if (line == 0) {
                        printf(" %-*ld |", column_width, values[col].int_value);
                    } else {
                        printf(" %-*s |", column_width, ""); // Spațiu gol
                    }
                    break;
                }
                default:
                    if (line == 0) {
                        printf(" %-*s |", column_width, "UNKNOWN");
                    } else {
                        printf(" %-*s |", column_width, "");
                    }
                    break;
            }
        }
    }
    
cleanup:
    // Eliberăm memoria alocată pentru string-uri
    for (uint64_t col = 0; col < metadata->num_columns; col+=1) {
        if (values[col].type == TYPE_VARCHAR && values[col].str_value != NULL) {
            free(values[col].str_value);
            values[col].str_value = NULL;
        }
    }

    // Eliberăm memoria pentru conținutul rândului
    if (row_content_mem) {
        free(row_content_mem);
    }
}

void print_separator(int num_columns) {
    int column_width = 20;  // Aceeași lățime ca în display_table_anthet
    
    for (int i = 0; i < num_columns; i+=1) {
        printf("+");
        for (int j = 0; j < column_width + 2; j+=1) {
            printf("-");
        }
    }
    printf("+\n");
}

int* display_table_anthet(char **columns, int num_columns, MetadataPage *meta) {
    int column_width = 20;  // Folosește aceeași lățime pentru toate coloanele

    int* column_indexes = malloc(num_columns * sizeof(int));
    
    if (!column_indexes) {
        fprintf(stderr, "Memory allocation failed for column indexes\n");
        return NULL;
    }

    bool column_found = false;
    for (int i = 0; i < num_columns; i+=1) {
        column_found = false;
        for (uint32_t j = 0; j < meta->num_columns; j+=1) {
            if (strcmp(columns[i], meta->column_names[j]) == 0) {
                column_found = true;
                column_indexes[i] = j;
                break;
            }
        }
        if (!column_found) {
            printf("        Column %s not found in table %s\n", columns[i], meta->table_name);
            return NULL;
        }
    }

    printf("\n");
    print_separator(num_columns);


    for (int i = 0; i < num_columns; i+=1) {
        printf("| %-*s ", column_width, columns[i]);
    }
    printf("|\n");

    print_separator(num_columns);

    return column_indexes;
}


void display_all_rows(RowNode *node, MetadataPage *metadata, int* column_indexes, int num_columns) {
    if (node == NULL) {
        return;
    }
    
    // Verificăm nodurile duplicate pentru a evita cicluri infinite
    static void* visited_nodes[MAX_VISITED_NODES] = {0};  // Stocăm ultimele 1000 noduri vizitate
    static int visited_count = 0;
    
    // Verifică dacă nodul a fost deja vizitat
    for (int v = 0; v < visited_count; v+=1) {
        if (visited_nodes[v] == node) {
            // Am detectat un ciclu, oprim traversarea
            return;
        }
    }
    
    // Adaugă nodul la lista de noduri vizitate
    if (visited_count < MAX_VISITED_NODES) {
        visited_nodes[visited_count+=1] = node;
    } else {
        // Resetăm array-ul dacă depășim limita
        visited_count = 0;
        visited_nodes[visited_count+=1] = node;
    }

    // Afișează rândurile din nodul curent
    for (int i = 1; i <= node->num_keys && i < ROW_MAX_KEYS; i+=1) {
        uint64_t offset = (uint64_t)node->raw_data[i];
        
        // Verifică dacă offsetul e valid
        if (offset == 0 || offset >= df->size) {
            // Offset invalid, trecem la următorul rând
            continue;
        }
        
        void *row_content = df->start_ptr + offset;
        
        // Verificare rapidă a dimensiunii rândului
        uint64_t row_size;
        memcpy(&row_size, row_content, sizeof(uint64_t));
        
        // Ignoră rândurile cu dimensiune invalidă
        if (row_size < 9 || row_size > MAX_BUFFER_SIZE) {
            continue;
        }

        printf("|");
        print_row_content(row_content, metadata, column_indexes, num_columns);
        printf("\n");
        
        print_separator(num_columns);
    }

    // Traversează recursiv nodurile copil, cu verificări de siguranță
    if (node->plink != NULL) {
        for (int i = 0; i <= node->num_keys && i < ROW_MAX_KEYS; i+=1) {
            if (node->plink[i] != NULL) {
                // Verificări suplimentare de validitate
                if ((uintptr_t)node->plink[i] < 1000 || 
                    (uintptr_t)node->plink[i] > (uintptr_t)df->start_ptr + df->size) {
                    // Pointer suspect sau în afara limitelor, ignoră
                    continue;
                }
                
                display_all_rows(node->plink[i], metadata, column_indexes, num_columns);
            }
        }
    }
    
    // Când ieșim din recursivitate, eliminăm nodul din lista de vizitate
    if (visited_count > 0) {
        visited_count--;
    }
}

void display_table(char **columns, int num_columns, MetadataPage *meta, Statement *stmt) {
    int *column_indexes = display_table_anthet(columns, num_columns, meta);

    if (column_indexes == NULL) {
        return;
    }
    
    RowNode *node = root;
    display_all_rows(node, meta, column_indexes, num_columns);
    printf("\n");

    free(column_indexes);
}




void set_table_parameters(MetadataPage *metadata, Statement *stmt) { 
    metadata->root_page_num = 0;   
    metadata->last_table_id = global_id;
    
    // printf("Setting table parameters...\n");
    metadata->magic = MAGIC_NUMBER;
    metadata->num_columns = stmt->createStmt.num_columns;
    strcpy(metadata->table_name, stmt->createStmt.table);
    for (int i = 0; i < stmt->createStmt.num_columns; i+=1) {

        strcpy(metadata->column_names[i], stmt->createStmt.columns[i].column_name);
        metadata->column_sizes[i] = stmt->createStmt.columns[i].length;
        if (strcasecmp(stmt->createStmt.columns[i].type, "Int") == 0) {
            metadata->column_types[i] = TYPE_INT;
            metadata->column_sizes[i] = sizeof(int);
        } 
        else if (strcasecmp(stmt->createStmt.columns[i].type, "float") == 0) {
            metadata->column_types[i] = TYPE_FLOAT;
            metadata->column_sizes[i] = sizeof(float);
        } 
        else if (strncasecmp(stmt->createStmt.columns[i].type, "String", 7) == 0) {
            metadata->column_types[i] = TYPE_VARCHAR;
            // Pentru VARCHAR, dimensiunea este specificată în createStmt->columns[i].length
            metadata->column_sizes[i] = stmt->createStmt.columns[i].length;
        } 
        else if (strcasecmp(stmt->createStmt.columns[i].type, "timestamp") == 0) {
            metadata->column_types[i] = TYPE_TIMESTAMP;
            metadata->column_sizes[i] = sizeof(uint64_t); // Timestamp stocat ca un întreg pe 64 de biți
        } 
        else {
            // // Tip necunoscut, setează implicit la INT
            // printf("Warning: Unknown type '%s' for column '%s', defaulting to INT\n", 
            //     stmt->createStmt.columns[i].type, stmt->createStmt.columns[i].column_name);
            metadata->column_types[i] = TYPE_INT;
            metadata->column_sizes[i] = sizeof(int);
        }
        if(stmt->createStmt.columns[i].constraint == CONSTRAINT_NOT_NULL) {
            metadata->column_constraints[i] = CONSTRAINT_NOT_NULL;
        } else if(stmt->createStmt.columns[i].constraint == CONSTRAINT_UNIQUE) {
            metadata->column_constraints[i] = CONSTRAINT_UNIQUE;
        } else if(stmt->createStmt.columns[i].constraint == CONSTRAINT_PRIMARY_KEY) {
            metadata->column_constraints[i] = CONSTRAINT_PRIMARY_KEY;
        } else if(stmt->createStmt.columns[i].constraint == CONSTRAINT_FOREIGN_KEY) {
            metadata->column_constraints[i] = CONSTRAINT_FOREIGN_KEY;
        } else {
            metadata->column_constraints[i] = CONSTRAINT_NONE;
        }
    }
}

bool is_data_in_row(int column_index, void* row_content,Statement* stmt) {
        for(int col = 0; col < metadata->num_columns; col+=1){
            if(metadata->column_types[col] == TYPE_VARCHAR){
                uint32_t string_length;
                memcpy(&string_length, row_content, sizeof(uint32_t));
                row_content += sizeof(uint32_t);

                if(col == column_index){
                    void* string_content = malloc(string_length+1);
                    memcpy(string_content, row_content, string_length);
                    ((char*)string_content)[string_length] = '\0';

                    if(strcmp(string_content,stmt->insertStmt.values[column_index].value) == 0){
                        free(string_content);
                        return true;
                    }
                    free(string_content);
                    return false;
                }
                row_content += string_length;
                
            } else if(metadata->column_types[col] == TYPE_INT){
                int64_t value;
                memcpy(&value, row_content, sizeof(int64_t));
                row_content += sizeof(int64_t);

                if(col == column_index){
                    if(value == strtoll(stmt->insertStmt.values[column_index].value, NULL, 10)){
                        return true;
                    }
                    return false;
                }
            } 
        }
}

bool constraint_unique(RowNode *node, Statement*stmt, MetadataPage *metadata,int column_index) {
    if(node == NULL){
        perror("Error: NULL node in constraint_unique");
        return false;
    }

    for(int i=1; i<= node->num_keys; i+=1){
        uint64_t offset = (uint64_t)node->raw_data[i];
        if(offset == 0 || offset >= df->size){
            continue;
        }
        void *row_content = df->start_ptr + offset;

        uint64_t row_byte_index = 0;

        if(is_data_in_row(column_index, row_content,stmt)){
            return true; 
        }

    }

    if(node->plink != NULL){
        for(int i=0; i<= node->num_keys; i+=1){
            if(node->plink[i] != NULL){
                constraint_unique(node->plink[i],stmt,metadata,column_index);
            }
        }
    }

    return false;
}