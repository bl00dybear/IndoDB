#ifndef PARSER_STRUCTURES_H
#define PARSER_STRUCTURES_H

#include "../constraints.h"

typedef enum {
    STATEMENT_INSERT,
    STATEMENT_SELECT,
    STATEMENT_CREATE,
    STATEMENT_DROP,
    STATEMENT_UPDATE,
    STATEMENT_DELETE,
    STATEMENT_CREATE_DB,
    STATEMENT_DROP_DB,
    STATEMENT_USE_DB,
    STATEMENT_SHOW_DB,
    STATEMENT_SHOW_TB,
    STATEMENT_DESC_TB,
} StatementType;

// typedef enum {
//     CONSTRAINT_NONE,
//     CONSTRAINT_NOT_NULL,
//     CONSTRAINT_UNIQUE,
//     CONSTRAINT_PRIMARY_KEY,
//     CONSTRAINT_FOREIGN_KEY,
// } ConstraintType;

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
    char **cond_column;
    int num_cond_columns;
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
    char **set_columns;
    int num_set_columns;
    char **set_values;
    int num_set_values;
    char *table;
    char *condition;
    char **cond_column;
    int num_cond_columns;
} UpdateStmtStruct;

typedef struct {
    char *table;
    char *condition;
    char **cond_column;
    int num_cond_columns;
} DeleteStmtStruct;

typedef struct {
    char *database;
} CreateDbStmtStruct;

typedef struct {
    char *database;
} DropDbStmtStruct;

typedef struct {
    char *database;
} UseDbStmtStruct;

typedef struct {
} ShowDbStmtStruct;

typedef struct {
} ShowTbStmtStruct;

typedef struct {
    char *table;
} DescTbStmtStruct;

typedef struct {
    StatementType type;
    union {
        InsertStmtStruct insertStmt;
        SelectStmtStruct selectStmt;
        CreateStmtStruct createStmt;
        DropStmtStruct dropStmt;
        UpdateStmtStruct updateStmt;
        DeleteStmtStruct deleteStmt;
        CreateDbStmtStruct createDbStmt;
        DropDbStmtStruct dropDbStmt;
        UseDbStmtStruct useDbStmt;
        ShowDbStmtStruct showDbStmt;
        ShowTbStmtStruct showTbStmt;
        DescTbStmtStruct descTbStmt;
    };
} Statement;

#endif

// INSERT INTO tabela (col_1, col_2) VALUES (1, 'aaaa');