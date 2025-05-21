#ifndef PARSER_STRUCTURES_H
#define PARSER_STRUCTURES_H

#include "../constraints.h"

typedef enum {
    STATEMENT_INSERT,
    STATEMENT_SELECT,
    STATEMENT_CREATE,
    STATEMENT_DROP,
    STATEMENT_CREATE_DB,
    STATEMENT_DROP_DB,
    STATEMENT_USE_DB,
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
    char *db_name;
} CreateDbStmtStruct;

typedef struct {
    char *db_name;
} DropDbStmtStruct;

typedef struct {
    char *db_name;
} UseDbStmtStruct;

typedef struct {
    StatementType type;
    union {
        InsertStmtStruct insertStmt;
        SelectStmtStruct selectStmt;
        CreateStmtStruct createStmt;
        DropStmtStruct dropStmt;
        CreateDbStmtStruct createDbStmt;
        DropDbStmtStruct dropDbStmt;
        UseDbStmtStruct useDbStmt;
    };
} Statement;

#endif

// INSERT INTO tabela (col_1, col_2) VALUES (1, 'aaaa');