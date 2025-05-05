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

void parse_statement(const char *filename, Statement *stmt) {
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
        exit(1);
    }

    // check for errors
    cJSON *error_ptr = cJSON_GetObjectItemCaseSensitive(json, "error");
    if(error_ptr) {
        // gracefully display the error
        printf("%s\n", error_ptr->valuestring);
        cJSON_Delete(json);
        free(data);
        return;
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
    } else {
        printf("Unknown statement type: %s\n", statement_type->valuestring);
        exit(1);
    }

    cJSON_Delete(json);
    free(data);
}

void process_statement(Statement *stmt) {
    switch (stmt->type) {
        case STATEMENT_INSERT: {
            uint64_t row_size = 0;
            char* row_content = get_row_content(stmt,&row_size);
            printf("Row content: %ld\n",row_size);
            void *written_address = write_row(df, row_content, row_size);
            const uint64_t current_id = global_id++;
            insert(current_id,written_address);
            set_file_dirty_db(db, true);
            set_file_dirty_df(df,true);


            commit_changes_db(db);
            commit_changes_df(df);
        }
        case STATEMENT_SELECT: {
            if (!strcmp(stmt -> selectStmt.columns[0],"*")) {
                // printf("Selecting all columns\n");
                traversal(root);
                print_entire_table(root,df);
            }else {
             // TODO : select specified columns
            }
        }
        default: ;
    }
}

int cli() {
    char input[MAX_INPUT_SIZE];
    char line[MAX_INPUT_SIZE];

    // printf("\033[H\033[J");

    while (1) {
        printf("IndoDB> ");
        input[0] = '\0';

        while (fgets(line, sizeof(line), stdin)) {
            // printf("A luat input\n\n");
            size_t len = strlen(line);
            if (len > 0 && line[len - 1] == '\n') {
                line[len - 1] = '\0';
                len--;
            }

            if (strcmp(line, "EXIT;") == 0) {
                printf("Exiting IndoDB...\n");
                return 0;
            }

            if (strcmp(line, "CLEAR;") == 0) {
                printf("\033[H\033[J");
                printf("IndoDB> ");
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
            while (trimmed >= input && *trimmed == ' ') {
                trimmed--;
            }
            if (*trimmed == ';') {
                break;
            }

            printf("     -> ");
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
            parse_statement("../output/output.json", stmt);

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
            }


            if (stmt != NULL) {
                process_statement(stmt);
                if (stmt->type == STATEMENT_INSERT) {
                    printf("Parsed an INSERT statement!\n");
                } else if (stmt->type == STATEMENT_SELECT) {
                    printf("Parsed a SELECT statement!\n");
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
    printf("%ld\n", root->page_num);
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



void database_init() {
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

    if(!(~access(DB_FILENAME, F_OK)))
        create_database_file(DB_FILENAME);

    if(!(~access(DATA_FILENAME, F_OK)))
        create_database_file(DATA_FILENAME);

    open_database_file(&db->fd,DB_FILENAME);
    open_database_file(&df->fd,DATA_FILENAME);

    get_db_file_size(db);
    get_df_file_size(df);

    if(!db->size)
        init_create_db_memory_block(db);
    // else {
    //     root = load_btree_from_disk(db);
    //     printf("%ld\n", root->page_num);
    //     if (root) {
    //         printf("B-Tree successfully loaded!\n");
    //     } else {
    //         printf("Failed to load B-Tree.\n");
    //     }
    // }

    if(!df->size)
        init_create_df_memory_block(df);
    // else
    //     load_datafile(df);

    printf("File size %ld\n",db->size);

    memory_map_db_file(db);
    memory_map_df_file(df);

    set_file_dirty_db(db, false);
    set_file_dirty_df(df, false);
    set_new_file_free_blocks(db);

    printf("%d %ld\n",db->fd,db->size);

    free_page_queue = create_queue();

    for(int i=1;i<=10000;i+=1){
        push(free_page_queue,i);
    }
}

bool is_database_empty() {
    typedef struct {
        uint64_t write_ptr_offset;
        uint64_t magic;
    } DataFileHeader;

    DataFileHeader header;
    memcpy(&header, df->start_ptr, sizeof(header));

    return header.magic != MAGIC_NUMBER;
}


int main() {
    database_init();

    if (!is_database_empty()) {
        printf("Database is not empty!\n");
        database_load();
    }

    return cli();
}