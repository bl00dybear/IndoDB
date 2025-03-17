#ifndef EE4AC2EA_E38F_4F62_9377_375DE5CE0648
#define EE4AC2EA_E38F_4F62_9377_375DE5CE0648

#include "libraries.h"

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
    uint64_t raw_data[169]; 
    uint64_t link[170];
    struct RowNode *plink[170];    //21 liberi

}RowNode;

RowNode *root;

typedef struct QueueNode {
    int data;
    struct QueueNode* next;
} QueueNode;

typedef struct {
    QueueNode* front;  // Primul element (capul cozii)
    QueueNode* rear;   // Ultimul element (coada cozii)
} Queue;

Queue* free_page_queue ;


#endif /* EE4AC2EA_E38F_4F62_9377_375DE5CE0648 */
