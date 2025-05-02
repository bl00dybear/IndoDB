#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cJSON.h"

#define MAX_INPUT_SIZE 1024

typedef enum {
    STATEMENT_INSERT,
    STATEMENT_SELECT,
} StatementType;

typedef struct {
    char **columns;
    int num_columns;
    char *table;
    struct {
        char *value;
        char *valueType;
    } *values;
    int num_values;
} InsertStmtStruct;

typedef struct {
    char **columns;
    int num_columns;
    char *table;
    char *condition;
} SelectStmtStruct;

typedef struct {
    StatementType type;
    union {
        InsertStmtStruct insertStmt;
        SelectStmtStruct selectStmt;
    };
} Statement;

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
            // Parse input through sql_parser
            FILE *fp = popen("./src/parser/sql_parser", "w");
            if (fp == NULL) {
                perror("Error opening pipe");
                return 1;
            }
            fprintf(fp, "%s\n", input);
            pclose(fp);
        
            // Initialize Statement struct from output.json
            Statement stmt;
            parse_statement("./src/output.json", &stmt);
        
            // Test the parsed struct
            if (stmt.type == STATEMENT_INSERT) {
                printf("Parsed an INSERT statement!\n");
            } else if (stmt.type == STATEMENT_SELECT) {
                printf("Parsed a SELECT statement!\n");
            }
            free_statement(&stmt);
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


int main() {
    return cli();
}