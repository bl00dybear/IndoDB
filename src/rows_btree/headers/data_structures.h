#ifndef DATA_STRUCTURES_H
#define DATA_STRUCTURES_H

#include "libraries.h"
#include "config.h"
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
    uint64_t keys[169];
    void* raw_data[169];            //also 8 bytes
    uint64_t link[170];
    struct RowNode *plink[170];    //21 liberi
}RowNode;

extern RowNode *root;

typedef struct QueueNode {
    int data;
    struct QueueNode* next;
} QueueNode;

typedef struct {
    QueueNode* front;  // Primul element (capul cozii)
    QueueNode* rear;   // Ultimul element (coada cozii)
} Queue;

extern Queue* free_page_queue;


typedef struct {
    uint64_t page_num;
    RowNode* node;
} NodeEntry;

extern NodeEntry visited_nodes[MAX_NODES];
extern int visited_count;

extern uint64_t serialized_pages[MAX_NODES];
extern int serialized_count;

typedef struct DataFile {
    int fd;
    ssize_t size;
    void *write_ptr;
    bool dirty;
}DataFile;

extern uint64_t global_id;


#endif