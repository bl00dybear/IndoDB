#ifndef PARSER_STRUCTURES_H
#define PARSER_STRUCTURES_H

typedef enum {
    STATEMENT_INSERT,
    STATEMENT_SELECT,
    STATEMENT_CREATE,
    STATEMENT_DROP,
} StatementType;

typedef enum {
    CONSTRAINT_NONE,
    CONSTRAINT_PRIMARY_KEY,
    CONSTRAINT_FOREIGN_KEY,
    CONSTRAINT_NOT_NULL,
} ConstraintType;

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
    struct {
        char *column_name;
        ConstraintType constraint;
        char *type;
        int length;
    } *columns;
    int num_columns;
    char *table;
} CreateStmtStruct;

typedef struct {
    char *table;
} DropStmtStruct;

typedef struct {
    StatementType type;
    union {
        InsertStmtStruct insertStmt;
        SelectStmtStruct selectStmt;
        CreateStmtStruct createStmt;
        DropStmtStruct dropStmt;
    };
} Statement;


#endif

// INSERT INTO tabela (col_1, col_2) VALUES (1, 'aaaa');