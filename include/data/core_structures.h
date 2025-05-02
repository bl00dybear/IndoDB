#ifndef CORE_STRUCTURES_H
#define CORE_STRUCTURES_H

#include "../libraries.h"
#include "../config.h"
#include <stdio.h>   

typedef struct DBFile{
    int fd;         // File descriptor for the database
    ssize_t size;    // Size of the mapped file
    void *data;     // Pointer to the mapped memory region
    bool dirty;      // Flag to track if changes were made
    int free_blocks; // Number of free blocks
}DBFile;

typedef struct RowNode{
    uint8_t is_leaf;
    uint16_t num_keys;
    uint64_t page_num;
    uint64_t keys[ROW_MAX_KEYS];
    void* raw_data[ROW_MAX_KEYS];            //also 8 bytes
    uint64_t link[ROW_MAX_KEYS+1];
    struct RowNode *plink[ROW_MAX_KEYS+1];    //21 free bytes
}RowNode;

extern RowNode *root;

typedef struct QueueNode {
    int data;
    struct QueueNode* next;
} QueueNode;

typedef struct {
    QueueNode* front;
    QueueNode* rear;
} Queue;

extern Queue* free_page_queue;


typedef struct {
    uint64_t page_num;
    RowNode* node;
} NodeEntry;

extern NodeEntry visited_nodes[MAX_VISITED_NODES];
extern int visited_count;

extern uint64_t serialized_pages[MAX_VISITED_NODES];
extern int serialized_count;

typedef struct DataFile {
    int fd;
    void *start_ptr;
    ssize_t size;
    void *write_ptr;
    bool dirty;
}DataFile;

extern uint64_t global_id;


#endif