#ifndef INITAL_PAGE_HANDLING_H
#define INITAL_PAGE_HANDLING_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>

#define PAGE_SIZE 4096  // 4 KiB pages
#define INITIAL_DB_SIZE (PAGE_SIZE * 2)  // Initial size (10 pages)
#define DB_FILENAME "database.bin"
#define JOURNAL_FILENAME "journal.bin"

// Database structure with memory mapping
typedef struct {
    int fd;         // File descriptor for the database
    ssize_t size;    // Size of the mapped file
    void *data;     // Pointer to the mapped memory region
    int dirty;      // Flag to track if changes were made
} DBFile;


#endif //INITAL_PAGE_HANDLING_H
