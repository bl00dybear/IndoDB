#ifndef CONFIG_H
#define CONFIG_H

#define PAGE_SIZE 4096
#define INITIAL_DB_SIZE PAGE_SIZE
#define BLOCK_SIZE PAGE_SIZE
#define ROW_MAX_KEYS 169
#define ROW_MIN_KEYS 86
#define DB_FILENAME "../database/btrees.bin"
#define DATA_FILENAME "../database/data.bin"

#define MAX_VISITED_NODES 100000
#define MAGIC_NUMBER  0x494E444F4442
#define MAX_INPUT_SIZE 102400

#define MAX_BUFFER_SIZE 102400
#define INT_MAX INT8_MAX
#define INT_MIN INT8_MIN


#endif
