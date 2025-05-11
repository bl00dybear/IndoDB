#include "../include/data/parser_structures.h"
#include "../include/libraries.h"
#include "../include/utils/cJSON.h"
#include "../include/config.h"
#include "../include/core/row_btree_ops.h"
#include "../include/core/datafile_ops.h"
#include "../include/core/memory_ops.h"
#include "../include/data/core_structures.h"
#include "../include/utils/globals.h"
#include "../include/utils/queue.h"


void disable_raw_mode() {
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

void enable_raw_mode() {
    tcgetattr(STDIN_FILENO, &orig_termios);
    atexit(disable_raw_mode);
    struct termios raw = orig_termios;
    raw.c_lflag &= ~(ECHO | ICANON | ISIG);
    raw.c_iflag &= ~(IXON | ICRNL);
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}


int read_line(char *buf, size_t size) {
    int len = 0, pos = 0, hist_pos = hist_len;
    char c;
    enable_raw_mode();
    while (1) {
        if (read(STDIN_FILENO, &c, 1) != 1) {
            disable_raw_mode();
            return 0; // EOF
        }
        if (c == '\r' || c == '\n') {
            write(STDOUT_FILENO, "\r\n", 2);
            buf[len] = '\0';
            if (len > 0 && hist_len < MAX_HISTORY) {
                history[hist_len++] = strdup(buf);
            }
            disable_raw_mode();
            return 1;
        } else if (c == 127) {
            if (pos > 0) {
                memmove(buf + pos - 1, buf + pos, len - pos);
                len--; pos--;
                write(STDOUT_FILENO, "\x1b[D", 3);
                write(STDOUT_FILENO, buf + pos, len - pos);
                write(STDOUT_FILENO, " ", 1);
                for (int i = 0; i <= len - pos; i++) write(STDOUT_FILENO, "\x1b[D", 3);
            }
        } else if (c == '\x1b') {
            char seq[2];
            if (read(STDIN_FILENO, &seq[0], 1) != 1) continue;
            if (read(STDIN_FILENO, &seq[1], 1) != 1) continue;
            if (seq[0] == '[') {
                if (seq[1] == 'C' && pos < len) { write(STDOUT_FILENO, "\x1b[C", 3); pos++; }
                else if (seq[1] == 'D' && pos > 0) { write(STDOUT_FILENO, "\x1b[D", 3); pos--; }
                else if (seq[1] == 'A' && hist_pos > 0) {
                    hist_pos--;
                    // clear line
                    while (pos--) write(STDOUT_FILENO, "\x1b[D", 3);
                    for (int i = 0; i < len; i++) write(STDOUT_FILENO, " ", 1);
                    for (int i = 0; i < len; i++) write(STDOUT_FILENO, "\x1b[D", 3);
                    const char *h = history[hist_pos];
                    len = pos = strlen(h);
                    strcpy(buf, h);
                    write(STDOUT_FILENO, buf, len);
                } else if (seq[1] == 'B' && hist_pos < hist_len) {
                    hist_pos++;
                    // clear line
                    while (pos--) write(STDOUT_FILENO, "\x1b[D", 3);
                    for (int i = 0; i < len; i++) write(STDOUT_FILENO, " ", 1);
                    for (int i = 0; i < len; i++) write(STDOUT_FILENO, "\x1b[D", 3);
                    if (hist_pos < hist_len) {
                        const char *h = history[hist_pos];
                        len = pos = strlen(h);
                        strcpy(buf, h);
                        write(STDOUT_FILENO, buf, len);
                    } else {
                        len = pos = 0;
                        buf[0] = '\0';
                    }
                }
            }
        } else if (c >= 32 && c <= 126) {
            if (len < (int)size - 1) {
                memmove(buf + pos + 1, buf + pos, len - pos);
                buf[pos] = c;
                write(STDOUT_FILENO, buf + pos, len - pos + 1);
                len++; pos++;
                for (int i = 0; i < len - pos; i++) write(STDOUT_FILENO, "\x1b[D", 3);
            }
        }
    }
}

void free_statement(Statement *stmt) {
    if (stmt->type == STATEMENT_INSERT) {
        for (int i = 0; i < stmt->insertStmt.num_columns; i++) {
            free(stmt->insertStmt.columns[i]);
        }
        free(stmt->insertStmt.columns);

        for (int i = 0; i < stmt->insertStmt.num_values; i++) {
            free(stmt->insertStmt.values[i].value);
            free(stmt->insertStmt.values[i].valueType);
        }
        free(stmt->insertStmt.values);

        free(stmt->insertStmt.table);
    } else if (stmt->type == STATEMENT_SELECT) {
        for (int i = 0; i < stmt->selectStmt.num_columns; i++) {
            free(stmt->selectStmt.columns[i]);
        }
        free(stmt->selectStmt.columns);
        free(stmt->selectStmt.table);
        if (stmt->selectStmt.condition) {
            free(stmt->selectStmt.condition);
        }
    }
}

int parse_statement(const char *filename, Statement *stmt) {
    // Este deschis fisierul output.json
    FILE *fp = fopen(filename, "r");
    // Error Handling
    if (!fp) {
        perror("Failed to open JSON file");
        exit(1);
    }

    // Cauta finalul fisierul
    fseek(fp, 0, SEEK_END);
    // Ia dimensiunea acestuia
    long length = ftell(fp);
    // Cauta inceputul fisierului
    fseek(fp, 0, SEEK_SET);
    // Aloca memorie pentru a stoca
    // continutul fisierului in data
    char *data = malloc(length + 1);
    // Citeste length bytes(1) in data
    fread(data, 1, length, fp);
    // Pune terminatorul de string
    data[length] = '\0';
    // Inchide fisierul
    fclose(fp);


    // Parsam data si stocam in json
    cJSON *json = cJSON_Parse(data);
    // Error Handling
    if (!json) {
        printf("Error parsing JSON\n");
        return 1; 
    }

    // check for errors
    cJSON *error_ptr = cJSON_GetObjectItemCaseSensitive(json, "error");
    if(error_ptr) {
        // gracefully display the error
        printf("%s\n", error_ptr->valuestring);
        cJSON_Delete(json);
        free(data);
        return -1;
    }

    // Selectam ce tip de statement este
    cJSON *statement_type = cJSON_GetObjectItemCaseSensitive(json, "statement");
    // printf("Tip de stmt: %s\n", statement_type->valuestring);

    // Initializam statementul in functie de ce tip este:
    if (strcmp(statement_type->valuestring, "InsertStmt") == 0) {
        stmt->type = STATEMENT_INSERT;

        cJSON *columns = cJSON_GetObjectItemCaseSensitive(json, "columns");
        cJSON *table = cJSON_GetObjectItemCaseSensitive(json, "table");
        cJSON *values = cJSON_GetObjectItemCaseSensitive(json, "values");

        int num_columns = cJSON_GetArraySize(columns);
        int num_values = cJSON_GetArraySize(values);

        stmt->insertStmt.num_columns = num_columns;
        stmt->insertStmt.num_values = num_values;
        // printf("Nr. coloane: %d\n", stmt->insertStmt.num_columns);
        // printf("Nr. valori: %d\n", stmt->insertStmt.num_values);

        stmt->insertStmt.columns = malloc(num_columns * sizeof(char*));
        for (int i = 0; i < num_columns; i++) {
            cJSON *col = cJSON_GetArrayItem(columns, i);
            stmt->insertStmt.columns[i] = strdup(col->valuestring);
            // printf("Coloana: %s\n", stmt->insertStmt.columns[i]);
        }

        stmt->insertStmt.table = strdup(table->valuestring);
        // printf("Nume tabel: %s\n", stmt->insertStmt.table);

        stmt->insertStmt.values = malloc(num_values * sizeof(*stmt->insertStmt.values));
        for (int i = 0; i < num_values; i++) {
            cJSON *val_obj = cJSON_GetArrayItem(values, i);
            cJSON *val = cJSON_GetObjectItemCaseSensitive(val_obj, "value");
            cJSON *valType = cJSON_GetObjectItemCaseSensitive(val_obj, "valueType");

            if (cJSON_IsString(val)) {
                stmt->insertStmt.values[i].value = strdup(val->valuestring);
                // printf("Value String: %s\n", stmt->insertStmt.values[i].value);
            } else if (cJSON_IsNumber(val)) {
                // se transforma int in char* pt ca asa e stocat in struct
                char buffer[32];
                snprintf(buffer, sizeof(buffer), "%g", val->valuedouble);
                stmt->insertStmt.values[i].value = strdup(buffer);
                // printf("Value Int: %s\n", stmt->insertStmt.values[i].value);
            } else {
                printf("Unsupported value type in JSON!\n");
                exit(1);
            }

            stmt->insertStmt.values[i].valueType = strdup(valType->valuestring);
            // printf("Value Type: %s\n", stmt->insertStmt.values[i].valueType);
        }
    } else if (strcmp(statement_type->valuestring, "SelectStmt") == 0) {
        stmt->type = STATEMENT_SELECT;

        cJSON *columns = cJSON_GetObjectItemCaseSensitive(json, "columns");
        cJSON *table = cJSON_GetObjectItemCaseSensitive(json, "table");
        cJSON *condition = cJSON_GetObjectItemCaseSensitive(json, "condition");

        int num_columns = cJSON_GetArraySize(columns);

        stmt->selectStmt.num_columns = num_columns;
        // printf("Nr. coloane: %d\n", stmt->selectStmt.num_columns);
        
        stmt->selectStmt.columns = malloc(num_columns * sizeof(char*));
        for (int i = 0; i < num_columns; i++) {
            cJSON *col = cJSON_GetArrayItem(columns, i);
            stmt->selectStmt.columns[i] = strdup(col->valuestring);
            // printf("Coloana: %s\n", stmt->selectStmt.columns[i]);
        }

        stmt->selectStmt.table = strdup(table->valuestring);
        // printf("Nume tabel: %s\n", stmt->selectStmt.table);

        if (cJSON_IsNull(condition)) {
            stmt->selectStmt.condition = NULL;
            // printf("Conditia e NULL: %d", stmt->selectStmt.condition);
        } else {
            stmt->selectStmt.condition = strdup(condition->valuestring);
        }
    } else if(strcmp(statement_type->valuestring, "CreateStmt") == 0) {
        stmt->type = STATEMENT_CREATE;

        cJSON *columns = cJSON_GetObjectItemCaseSensitive(json, "columns");
        cJSON *table = cJSON_GetObjectItemCaseSensitive(json, "table");
        int num_columns = cJSON_GetArraySize(columns);

        stmt->createStmt.num_columns = num_columns;
        stmt->createStmt.table = strdup(table->valuestring);


        stmt->createStmt.columns = malloc(num_columns * sizeof(*stmt->createStmt.columns));
        for(int i = 0; i < num_columns; i++) {
            cJSON *col_obj = cJSON_GetArrayItem(columns, i);
            if(!col_obj) {
                printf("Error: Column object is NULL\n");
                continue;
            }
            cJSON *col_name = cJSON_GetObjectItemCaseSensitive(col_obj, "name");
            stmt->createStmt.columns[i].column_name = strdup(col_name->valuestring);

            cJSON *col_constraint = cJSON_GetObjectItemCaseSensitive(col_obj, "constraint");
            if (cJSON_IsString(col_constraint)) {
                if (strcmp(col_constraint->valuestring, "PrimaryKey") == 0) {
                    stmt->createStmt.columns[i].constraint = CONSTRAINT_PRIMARY_KEY;
                } else if (strcmp(col_constraint->valuestring, "ForeignKey") == 0) {
                    stmt->createStmt.columns[i].constraint = CONSTRAINT_FOREIGN_KEY;
                } else if (strcmp(col_constraint->valuestring, "NotNull") == 0) {
                    stmt->createStmt.columns[i].constraint = CONSTRAINT_NOT_NULL;
                } else {
                    stmt->createStmt.columns[i].constraint = CONSTRAINT_NONE;
                }
            } else {
                stmt->createStmt.columns[i].constraint = CONSTRAINT_NONE;
            }

            cJSON *col_type_obj = cJSON_GetObjectItemCaseSensitive(col_obj, "type");
            // check if type is object
            if(!cJSON_IsObject(col_type_obj)) {
                stmt->createStmt.columns[i].type = strdup(col_type_obj->valuestring);

                // also length set to -1
                stmt->createStmt.columns[i].length = -1;
            } else{
                cJSON *col_type = cJSON_GetObjectItemCaseSensitive(col_type_obj, "type");
                if (cJSON_IsString(col_type)) {
                    stmt->createStmt.columns[i].type = strdup(col_type->valuestring);
                } else {
                    printf("Error: Column type is not a string\n");
                    stmt->createStmt.columns[i].type = NULL;
                }

                cJSON *col_type_length = cJSON_GetObjectItemCaseSensitive(col_type_obj, "length");
                if (cJSON_IsNumber(col_type_length)) {
                    stmt->createStmt.columns[i].length = col_type_length->valueint;
                } else {
                    printf("Error: Column length is not a number\n");
                    stmt->createStmt.columns[i].length = -1; // default value
                }
            }
        
        }
     } else if(strcmp(statement_type->valuestring, "DropStmt") == 0) {
        stmt->type = STATEMENT_DROP;

        cJSON *table = cJSON_GetObjectItemCaseSensitive(json, "table");
        stmt->dropStmt.table = strdup(table->valuestring);
     } else {
        printf("Unknown statement type: %s\n", statement_type->valuestring);
        return 1;
    }
    

    cJSON_Delete(json);
    free(data);
    return 0;
}

void process_statement(Statement *stmt) {
    // metadata = malloc(sizeof(MetadataPage));
    // if (!metadata) {
    //     perror("Metadata allocation failed");
    //     exit(1);
    // }
    
    switch (stmt->type) {
        case STATEMENT_INSERT: {
            database_init(stmt->insertStmt.table);
            if (!metadata) {
                printf("Error: Metadata initialization failed\n");
                break;
            }
            deserialize_metadata(db, metadata);

            printf("metadata->table_name: %s\n", metadata->table_name);
            printf("metadata ->num_columns: %d\n", metadata->num_columns);
            printf("metadata ->root_page_num: %ld\n", metadata->root_page_num);



            if(!strcmp(metadata->table_name,stmt->insertStmt.table)) {
                uint64_t row_size = 0;
                char* row_content = get_row_content(stmt,&row_size);
                printf("Row content: %ld\n",row_size);
                void *written_address = write_row(df, row_content, row_size);
                const uint64_t current_id = global_id++;
                insert(current_id,written_address);
                set_file_dirty_db(db, true);
                set_file_dirty_df(df,true);


                commit_changes_db(db, metadata);
                commit_changes_df(df);
            }else{
                printf("Table %s not found in database\n", stmt->insertStmt.table);
            }

            break;
        }
        case STATEMENT_SELECT: {
            database_init(stmt->selectStmt.table);
            if (!strcmp(stmt -> selectStmt.columns[0],"*")) {
                if(!strcmp(metadata->table_name,stmt->selectStmt.table)) {
                    print_entire_table(root,df,metadata,stmt);
                } else {
                    printf("Table %s not found in database\n", stmt->selectStmt.table);
                }
            }else {
             // TODO : select specified columns
            }

            break;
        }
        case STATEMENT_CREATE: {
            database_init(stmt->createStmt.table);
            printf("Database %s created successfully!\n", stmt->createStmt.table);

            set_table_parameters(metadata, stmt);
            printf("metadata->table_name: %s\n", metadata->table_name);
            printf("metadata ->num_columns: %d\n", metadata->num_columns);
            printf("metadata ->root_page_num: %ld\n", metadata->root_page_num);
            serialize_metadata(db, metadata);
            set_file_dirty_db(db, true);
            commit_changes_db(db, metadata);
                      

            break;
        }
        case STATEMENT_DROP: {
            char filepath_db[256] = {0};
            char filepath_df[256] = {0};
            
            // Build database and datafile paths
            strcpy(filepath_db, DB_FILENAME);
            strcat(filepath_db, stmt->dropStmt.table);
            strcat(filepath_db, ".bin");
            
            strcpy(filepath_df, DATA_FILENAME);
            strcat(filepath_df, stmt->dropStmt.table);
            strcat(filepath_df, ".bin");
            
            // Check if the files exist before attempting to delete
            if (access(filepath_db, F_OK) != 0 || access(filepath_df, F_OK) != 0) {
                printf("Table '%s' does not exist.\n", stmt->dropStmt.table);
                break;
            }
            
            // Free current resources if they're loaded
            if (db) {
                if (db->data) {
                    munmap(db->data, db->size);
                }
                close(db->fd);
                free(db);
                db = NULL;
            }
            
            if (df) {
                if (df->start_ptr) {
                    munmap(df->start_ptr, df->size);
                }
                close(df->fd);
                free(df);
                df = NULL;
            }
            
            if (metadata) {
                free(metadata);
                metadata = NULL;
            }
            
            if (free_page_queue) {
                destroy_queue(free_page_queue);
                free_page_queue = NULL;
            }
            
            // Reset global values
            root = NULL;
            visited_count = 0;
            serialized_count = 0;
            global_id = 1;
            
            // Try to remove the files
            int result_db = remove(filepath_db);
            int result_df = remove(filepath_df);
            
            if (result_db == 0 && result_df == 0) {
                printf("Table '%s' dropped successfully.\n", stmt->dropStmt.table);
            } else {
                printf("Error dropping table '%s'. ", stmt->dropStmt.table);
                if (result_db != 0) {
                    printf("Database file error\n");
                }
                if (result_df != 0) {
                    printf("Data file error\n");
                }
                printf("\n");
            }
            
            break;
        }
        // TODO : create table and drop table
        default: break;
    }
}

int cli() {
    char input[MAX_INPUT_SIZE];
    char line[MAX_INPUT_SIZE];

    // printf("\033[H\033[J");

    while (1) {
        printf("IndoDB> ");
        fflush(stdout);
        input[0] = '\0';

        while (read_line(line, sizeof(line))) {
            size_t len = strlen(line);
            if (len > 0 && line[len - 1] == '\n') {
                line[len - 1] = '\0'; len--;
            }

            if (strcmp(line, "EXIT;") == 0) {
                printf("Exiting IndoDB...\n");
                fflush(stdout);
                return 0;
            }

            if (strcmp(line, "CLEAR;") == 0) {
                printf("\033[H\033[J");
                printf("IndoDB> ");
                fflush(stdout);
                continue;
            }

            if (strlen(input) + strlen(line) + 2 < MAX_INPUT_SIZE) {
                strcat(input, line);
                strcat(input, " ");
            } else {
                printf("Input too long! Max supported length is %d characters.\n", MAX_INPUT_SIZE);
                break;
            }

            char *trimmed = input + strlen(input) - 1;
            while (trimmed >= input && *trimmed == ' ') trimmed--;
            if (*trimmed == ';') break;

            printf("     -> ");
            fflush(stdout);
        }


        if (strlen(input) > 0) {
            FILE *fp = popen("../output/sql_parser", "w");
            if (fp == NULL) {
                perror("Error opening pipe");
                return 1;
            }
            fprintf(fp, "%s\n", input);
            pclose(fp);


            Statement *stmt = malloc(sizeof(Statement));
            if (!stmt) {
                perror("malloc failed");
                exit(1);
            }
            int res = parse_statement("../output/output.json", stmt);
            if(res == -1){
                free_statement(stmt);
                continue;
            }

            // TESTEAZA AICI
            // TESTEAZA AICI
            if (stmt->type == STATEMENT_INSERT) {
                printf("INSERT into table: %s\n", stmt->insertStmt.table);
                for (int i = 0; i < stmt->insertStmt.num_columns; i++) {
                    printf("Column: %s\n", stmt->insertStmt.columns[i]);
                }
                for (int i = 0; i < stmt->insertStmt.num_values; i++) {
                    printf("Value: %s (Type: %s)\n",
                        stmt->insertStmt.values[i].value,
                        stmt->insertStmt.values[i].valueType);
                }
            } else if (stmt->type == STATEMENT_SELECT) {
                printf("SELECT from table: %s\n", stmt->selectStmt.table);
                for (int i = 0; i < stmt->selectStmt.num_columns; i++) {
                    printf("Column: %s\n", stmt->selectStmt.columns[i]);
                }
                if (stmt->selectStmt.condition) {
                    printf("Condition: %s\n", stmt->selectStmt.condition);
                } else {
                    printf("No condition\n");
                }
            } else if (stmt->type == STATEMENT_CREATE) {
                printf("CREATE table: %s\n", stmt->createStmt.table);
                for (int i = 0; i < stmt->createStmt.num_columns; i++) {
                    printf("Column: %s, Type: %s, Constraint: %d",
                        stmt->createStmt.columns[i].column_name,
                        stmt->createStmt.columns[i].type,
                        stmt->createStmt.columns[i].constraint);
                    if (stmt->createStmt.columns[i].length != -1) {
                        printf(", Length: %d", stmt->createStmt.columns[i].length);
                    }
                    printf("\n");
                }
            } else if (stmt->type == STATEMENT_DROP) {
                printf("DROP table: %s\n", stmt->dropStmt.table);
            } else {
                printf("Unknown statement type!\n");
            }

            if (stmt != NULL) {
                process_statement(stmt);


                if (stmt->type == STATEMENT_INSERT) {
                    printf("Parsed an INSERT statement!\n");
                } else if (stmt->type == STATEMENT_SELECT) {
                    printf("Parsed a SELECT statement!\n");
                } else if (stmt->type == STATEMENT_CREATE) {
                    printf("Parsed a CREATE statement!\n");
                } else if (stmt->type == STATEMENT_DROP) {
                    printf("Parsed a DROP statement!\n");
                } else {
                    printf("Unknown statement type!\n");
                }
                free_statement(stmt);
            }else {
                printf("Failed to parse statement!\n");
                exit(EXIT_FAILURE);
            }

        }
    }
}

void database_load() {
    root = load_btree_from_disk(db, metadata);
    printf("Root page number: %ld\n", root->page_num);
    printf("%ld\n\n", root->page_num);
    if (root) {
        printf("B-Tree successfully loaded!\n");
    } else {
        printf("Failed to load B-Tree.\n");
    }

    load_datafile(df);
}


void free_memory(Statement* stmt) {
    free_statement(stmt);

    // Free Btree
    // free_btree_node(root);

    if (free_page_queue) {
        destroy_queue(free_page_queue);
        free_page_queue = NULL;
    }

    if (db) {
        close(db->fd);
        free(db);
        db = NULL;
    }

    if (df) {
        close(df->fd);
        free(df);
        df = NULL;
    }

    root = NULL;
    visited_count = 0;
    serialized_count = 0;
    global_id = 1;
}

bool is_database_empty() {
    typedef struct {
        uint64_t magic;
        uint64_t write_ptr_offset;
    } DataFileHeader;

    DataFileHeader header;
    memcpy(&header, df->start_ptr, sizeof(header));

    return header.magic != MAGIC_NUMBER;
}



void database_init(char table_name[]) {
    db=NULL;
    df=NULL;
    metadata=NULL;
    free_page_queue=NULL;
    char dbfilepath[256] = {0};
    strcpy(dbfilepath, DB_FILENAME);
    strcat(dbfilepath, table_name); 
    strcat(dbfilepath, ".bin");
    printf("%s\n\n", dbfilepath);

    char datafilepath[256] = {0};
    strcpy(datafilepath, DATA_FILENAME);
    strcat(datafilepath, table_name);
    strcat(datafilepath, ".bin");
    printf("%s\n\n", datafilepath);

    if(!((db = malloc(sizeof(DBFile))))){
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!((df = malloc(sizeof(DataFile))))){
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!((metadata = malloc(sizeof(MetadataPage))))){
        perror("Metadata allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!(~access(dbfilepath, F_OK)))
        create_database_file(dbfilepath);
    
    

    if(!(~access(datafilepath, F_OK)))
        create_database_file(datafilepath);
    
    open_database_file(&db->fd,dbfilepath);
    open_database_file(&df->fd,datafilepath);
    
    get_db_file_size(db);
    get_df_file_size(df);

    if(!db->size)
        init_create_db_memory_block(db);

    if(!df->size)
        init_create_df_memory_block(df);

    printf("File size %ld\n",db->size);

    memory_map_db_file(db);
    memory_map_df_file(df);

    set_file_dirty_db(db, false);
    set_file_dirty_df(df, false);
    set_new_file_free_blocks(db);

    printf("%d %ld\n",db->fd,db->size);

    free_page_queue = create_queue();

    for(int i=1;i<=21792;i+=1){
        push(free_page_queue,i);
    }
    if (!is_database_empty()) {
        printf("Database is not empty!\n");
        database_load();
    }
    else {
        printf("Database is empty!\n");
    }
}



int main() {
    
    return cli();
}