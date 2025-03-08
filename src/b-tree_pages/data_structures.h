#ifndef A07437FB_09F9_4970_9687_F598D9165B3C
#define A07437FB_09F9_4970_9687_F598D9165B3C
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <sys/stat.h>

#define PAGE_SIZE 4096  // 4 KiB pages
#define NODE_MAX_KEYS 3  // Maximum number of keys a node can hold
#define NODE_MIN_KEYS 2  // Minimum number of keys a node can hold
#define INITIAL_DB_SIZE PAGE_SIZE  // Initial size (10 pages)
#define DB_FILENAME "database.bin"
#define JOURNAL_FILENAME "journal.bin"

typedef struct {
    int fd;         // File descriptor for the database
    ssize_t size;    // Size of the mapped file
    void *data;     // Pointer to the mapped memory region
    bool dirty;      // Flag to track if changes were made
}db_file;

struct node{
    int block_index[NODE_MAX_KEYS + 1];
    int count;
    struct node *link[NODE_MAX_KEYS + 1]; 
}node;

struct node *root;

#endif /* A07437FB_09F9_4970_9687_F598D9165B3C */
