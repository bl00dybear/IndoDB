#include "../include/cli/statement_parse.h"

void collect_columns_from_condition(cJSON *condition, char ***columns, int *count, int *capacity) {
    if (condition == NULL) return;

    cJSON *left = cJSON_GetObjectItemCaseSensitive(condition, "left");
    cJSON *right = cJSON_GetObjectItemCaseSensitive(condition, "right");
    cJSON *column = cJSON_GetObjectItemCaseSensitive(condition, "column");

    // If this node contains a column, it's a leaf condition
    if (column != NULL && cJSON_IsString(column)) {
        // Resize array if needed
        if (*count >= *capacity) {
            *capacity *= 2;
            *columns = realloc(*columns, (*capacity) * sizeof(char*));
        }
        (*columns)[(*count)++] = strdup(column->valuestring);
    }

    // Recurse into left and right if they exist
    if (left != NULL) collect_columns_from_condition(left, columns, count, capacity);
    if (right != NULL) collect_columns_from_condition(right, columns, count, capacity);
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

    // Initializam statementul in functie de ce tip este:
    if (strcmp(statement_type->valuestring, "InsertStmt") == 0) {
        stmt->type = STATEMENT_INSERT;

        cJSON *columns = cJSON_GetObjectItemCaseSensitive(json, "columns");
        cJSON *table = cJSON_GetObjectItemCaseSensitive(json, "table");
        cJSON *values = cJSON_GetObjectItemCaseSensitive(json, "values");

        int num_values = cJSON_GetArraySize(values);

        // Verificăm dacă "columns" este null sau absent
        int num_columns = 0;
        if (columns && cJSON_IsArray(columns)) {
            num_columns = cJSON_GetArraySize(columns);
            stmt->insertStmt.columns = malloc(num_columns * sizeof(char*));
            for (int i = 0; i < num_columns; i++) {
                cJSON *col = cJSON_GetArrayItem(columns, i);
                stmt->insertStmt.columns[i] = strdup(col->valuestring);
            }
        } else {
            stmt->insertStmt.columns = NULL;
        }

        stmt->insertStmt.num_columns = num_columns;
        stmt->insertStmt.num_values = num_values;

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
            stmt->selectStmt.cond_column = NULL;
            stmt->selectStmt.condition = NULL;
        } else {
            stmt->selectStmt.condition = cJSON_Duplicate(condition, 1);
            int capacity = 10000000;
            int count = 0;
            char **cond_columns = malloc(capacity * sizeof(char*));

            collect_columns_from_condition(condition, &cond_columns, &count, &capacity);

            stmt->selectStmt.cond_column = cond_columns;
            stmt->selectStmt.num_cond_columns = count;
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
                } else if (strcmp(col_constraint->valuestring, "Unique") == 0) {
                    stmt->createStmt.columns[i].constraint = CONSTRAINT_UNIQUE;
                } else {
                    printf("Error: Unknown constraint type: %s\n", col_constraint->valuestring);
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
    } else if (strcmp(statement_type->valuestring, "CreateDbStmt") == 0) {
        stmt->type = STATEMENT_CREATE_DB;
        
        cJSON *database = cJSON_GetObjectItemCaseSensitive(json, "database");
        stmt->createDbStmt.database = strdup(database->valuestring);
    } else if (strcmp(statement_type->valuestring, "DropDbStmt") == 0) {
        stmt->type = STATEMENT_DROP_DB;

        cJSON *database = cJSON_GetObjectItemCaseSensitive(json, "database");
        stmt->dropDbStmt.database = strdup(database->valuestring);
    } else if (strcmp(statement_type->valuestring, "UseDbStmt") == 0) {
        stmt->type = STATEMENT_USE_DB;

        cJSON *database = cJSON_GetObjectItemCaseSensitive(json, "database");
        stmt->useDbStmt.database = strdup(database->valuestring);
    } else if (strcmp(statement_type->valuestring, "ShowDbStmt") == 0) {
        stmt->type = STATEMENT_SHOW_DB;
    } else if (strcmp(statement_type->valuestring, "ShowTbStmt") == 0) {
        stmt->type = STATEMENT_SHOW_TB;
    } else if (strcmp(statement_type->valuestring, "DescTbStmt") == 0) {
        stmt->type = STATEMENT_DESC_TB;

        cJSON *table = cJSON_GetObjectItemCaseSensitive(json, "table");
        stmt->descTbStmt.table = strdup(table->valuestring);
    } else {
        printf("Unknown statement type: %s\n", statement_type->valuestring);
        return 1;
    }

    cJSON_Delete(json);
    free(data);
    return 0;
}