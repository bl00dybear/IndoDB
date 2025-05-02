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

void parse_statement(const char *filename, Statement *stmt) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Failed to open JSON file");
        exit(1);
    }

    fseek(fp, 0, SEEK_END);
    long length = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    char *data = malloc(length + 1);
    fread(data, 1, length, fp);
    data[length] = '\0';
    fclose(fp);

    cJSON *json = cJSON_Parse(data);
    if (!json) {
        printf("Error parsing JSON\n");
        exit(1);
    }

    cJSON *statement_type = cJSON_GetObjectItemCaseSensitive(json, "statement");
    if (strcmp(statement_type->valuestring, "InsertStmt") == 0) {
        stmt->type = STATEMENT_INSERT;

        cJSON *columns = cJSON_GetObjectItemCaseSensitive(json, "columns");
        cJSON *table = cJSON_GetObjectItemCaseSensitive(json, "table");
        cJSON *values = cJSON_GetObjectItemCaseSensitive(json, "values");

        int num_columns = cJSON_GetArraySize(columns);
        int num_values = cJSON_GetArraySize(values);

        stmt->insertStmt.num_columns = num_columns;
        stmt->insertStmt.num_values = num_values;

        stmt->insertStmt.columns = malloc(num_columns * sizeof(char*));
        for (int i = 0; i < num_columns; i++) {
            cJSON *col = cJSON_GetArrayItem(columns, i);
            stmt->insertStmt.columns[i] = strdup(col->valuestring);
        }

        stmt->insertStmt.table = strdup(table->valuestring);

        stmt->insertStmt.values = malloc(num_values * sizeof(*stmt->insertStmt.values));
        for (int i = 0; i < num_values; i++) {
            cJSON *val_obj = cJSON_GetArrayItem(values, i);
            cJSON *val = cJSON_GetObjectItemCaseSensitive(val_obj, "value");
            cJSON *valType = cJSON_GetObjectItemCaseSensitive(val_obj, "valueType");

            if (cJSON_IsString(val)) {
                stmt->insertStmt.values[i].value = strdup(val->valuestring);
            } else if (cJSON_IsNumber(val)) {
                char buffer[32];
                snprintf(buffer, sizeof(buffer), "%g", val->valuedouble);
                stmt->insertStmt.values[i].value = strdup(buffer);
            } else {
                printf("Unsupported value type in JSON!\n");
                exit(1);
            }

            stmt->insertStmt.values[i].valueType = strdup(valType->valuestring);
        }
    } else if (strcmp(statement_type->valuestring, "SelectStmt") == 0) {
        stmt->type = STATEMENT_SELECT;
        cJSON *columns = cJSON_GetObjectItemCaseSensitive(json, "columns");
        cJSON *table = cJSON_GetObjectItemCaseSensitive(json, "table");
        cJSON *condition = cJSON_GetObjectItemCaseSensitive(json, "condition");

        int num_columns = cJSON_GetArraySize(columns);
        stmt->selectStmt.num_columns = num_columns;

        stmt->selectStmt.columns = malloc(num_columns * sizeof(char*));
        for (int i = 0; i < num_columns; i++) {
            cJSON *col = cJSON_GetArrayItem(columns, i);
            stmt->selectStmt.columns[i] = strdup(col->valuestring);
        }

        stmt->selectStmt.table = strdup(table->valuestring);

        if (cJSON_IsNull(condition)) {
            stmt->selectStmt.condition = NULL;
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


void free_memory(const Statement* stmt) {
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
        case STATEMENT_SELECT: {}
        default: ;
    }
}


int cli() {
    char input[MAX_INPUT_SIZE];
    char line[MAX_INPUT_SIZE];

    // Clear screen before displaying prompt
    printf("\033[H\033[J");

    while (1) {
        printf("IndoDB> ");
        input[0] = '\0';

        while (fgets(line, sizeof(line), stdin)) {
            size_t len = strlen(line);
            if (len > 0 && line[len - 1] == '\n') {
                line[len - 1] = '\0';
                len--;
            }

            // Exit CLI command
            if (strcmp(line, "EXIT;") == 0) {
                printf("Exiting IndoDB...\n");
                return 0;
            }

            strcat(input, line);
            strcat(input, " ");

            // Check if the last non-space character is ';'
            char *trimmed = input + strlen(input) - 1;
            while (trimmed >= input && *trimmed == ' ') {
                trimmed--;
            }
            if (*trimmed == ';') {
                break;
            }

            printf("      > ");
        }

        if (strlen(input) > 0) {
            FILE *fp = popen("../output/sql_parser", "w");
            if (fp == NULL) {
                perror("Error opening pipe");
                return 1;
            }
            fprintf(fp, "%s\n", input);
            pclose(fp);

            Statement *stmt;
            parse_statement("../output/output.json",stmt);

            process_statement(stmt);
            // Test the parsed struct
            if (stmt->type == STATEMENT_INSERT) {
                printf("Parsed an INSERT statement!\n");
            } else if (stmt->type == STATEMENT_SELECT) {
                printf("Parsed a SELECT statement!\n");
            }
            free_statement(&stmt);
        }
    }
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





int main() {
    database_init();

    return cli();
}