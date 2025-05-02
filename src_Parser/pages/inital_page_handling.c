//
// Created by h4rapa1b on 3/7/25.
//

#include "inital_page_handling.h"

// Open the database file and map it into memory
DBFile* db_open(const char* filename) {
    DBFile* db = malloc(sizeof(DBFile));
    if (!db) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    // Open file with read/write, create if it doesn't exist
    db->fd = open(filename, O_RDWR | O_CREAT, 0644);
    if (db->fd == -1) {
        perror("Error opening database file");
        free(db);
        exit(EXIT_FAILURE);
    }

    // Get file size
    struct stat st;
    if (fstat(db->fd, &st) == -1) {
        perror("Error getting file size");
        free(db);
        exit(EXIT_FAILURE);
    }
    db->size = st.st_size;

    // If file is empty, set it to an initial size
    if (db->size == 0) {
        ftruncate(db->fd, INITIAL_DB_SIZE);
        db->size = INITIAL_DB_SIZE;
    }

    // Memory-map the file
    db->data = mmap(NULL, db->size, PROT_READ | PROT_WRITE, MAP_SHARED, db->fd, 0);
    if (db->data == MAP_FAILED) {
        perror("Error mapping database file");
        free(db);
        exit(EXIT_FAILURE);
    }

    db->dirty = 0;  // No changes yet
    return db;
}

// Close the database and unmap memory
void db_close(DBFile* db) {
    if (db) {
        msync(db->data, db->size, MS_SYNC);  // Ensure changes are written
        munmap(db->data, db->size);
        close(db->fd);
        free(db);
    }
}


// Read a page from memory-mapped file
void* db_read_page(DBFile* db, int page_num) {
    if (page_num * PAGE_SIZE >= db->size) {
        fprintf(stderr, "Page %d out of bounds!\n", page_num);
        return NULL;
    }
    return (void*)((char*)db->data + (page_num * PAGE_SIZE));
}

// Write data to a page in memory (marks it as dirty)
void db_write_page(DBFile* db, int page_num, void* new_data) {
    if (page_num * PAGE_SIZE >= db->size) {
        fprintf(stderr, "Page %d out of bounds!\n", page_num);
        return;
    }

    void* page = (char*)db->data + (page_num * PAGE_SIZE);
    memcpy(page, new_data, PAGE_SIZE);
    db->dirty = 1;  // Mark as dirty
}

// Commit transaction: Sync memory-mapped file to disk
void db_commit(DBFile* db) {
    if (db->dirty) {
        if (msync(db->data, db->size, MS_SYNC) == -1) {
            perror("Error syncing database to disk");
        } else {
            printf("Transaction committed successfully!\n");
            db->dirty = 0;  // Reset dirty flag
        }
    }
}

// Save a journal file before making changes (for rollback)
void db_start_transaction(DBFile* db) {
    FILE* journal = fopen(JOURNAL_FILENAME, "wb");
    if (!journal) {
        perror("Error opening journal file");
        return;
    }
    fwrite(db->data, db->size, 1, journal);  // Save snapshot
    fclose(journal);
    printf("Transaction started (journal created).\n");
}

// Rollback transaction: Restore from journal file
void db_rollback(DBFile* db) {
    FILE* journal = fopen(JOURNAL_FILENAME, "rb");
    if (!journal) {
        fprintf(stderr, "No journal file found! Cannot rollback.\n");
        return;
    }
    fread(db->data, db->size, 1, journal);  // Restore snapshot
    fclose(journal);
    remove(JOURNAL_FILENAME);  // Delete journal file
    printf("Transaction rolled back successfully!\n");

    db->dirty = 0;  // Reset dirty flag
}

