#ifndef PARSER_STRUCTURES_H
#define PARSER_STRUCTURES_H

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


#endif

// INSERT INTO tabela (col_1, col_2) VALUES (1, 'aaaa');