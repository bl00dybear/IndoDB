#ifndef CONFIG_H
#define CONFIG_H

#define PAGE_SIZE 4096
#define INITIAL_DB_SIZE PAGE_SIZE
#define BLOCK_SIZE PAGE_SIZE
#define ROW_MAX_KEYS 169
#define ROW_MIN_KEYS 86
extern char DB_FILENAME[256];
extern char DATA_FILENAME[256];

#define MAX_VISITED_NODES 100000
#define MAGIC_NUMBER  0x42444f444E49
#define MAX_INPUT_SIZE 65536

#define MAX_BUFFER_SIZE 1024
#define INT_MAX INT8_MAX
#define INT_MIN INT8_MIN

#define MAX_TABLE_NAME 64
#define MAX_COLUMNS 32
#define MAX_COLUMN_NAME 32
#define METADATA_SIZE 4096 

#define MAX_HISTORY 100

#define AST_PATH "../output/expr_cond"
#define AST_FILE_NAME "expr_cond"


#endif
