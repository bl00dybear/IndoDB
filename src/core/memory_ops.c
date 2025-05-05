#include "../../include/core/memory_ops.h"
#include "../../include/libraries.h"
#include "../../include/config.h"
#include "../../include/data/core_structures.h"

void create_database_file(char file_name[]){
    if (!(~creat(file_name, 0644))) {
        perror("Error creating database file");
        exit(EXIT_FAILURE);
    }
}

void open_database_file(int* db_filedescriptor, char file_name[]){
    if (!(~((*db_filedescriptor) = open(file_name, O_RDWR | O_CREAT, 0644)))) {
        perror("Error opening database file");
        exit(EXIT_FAILURE);
    }
}

void get_db_file_size(DBFile* db){
    struct stat st;
    if (!(~fstat(db->fd, &st))) {
        perror("Error getting file size");
        exit(EXIT_FAILURE);
    }
    db->size = st.st_size;
}

void get_df_file_size(DataFile* df){
    struct stat st;
    if (!(~fstat(df->fd, &st))) {
        perror("Error getting file size");
        exit(EXIT_FAILURE);
    }
    df->size = st.st_size;
}

void memory_map_db_file(DBFile* db){
    db->data = mmap(NULL, db->size, PROT_READ | PROT_WRITE, MAP_SHARED, db->fd, 0);
    if (db->data == MAP_FAILED) {
        perror("Error mapping database file");
        exit(EXIT_FAILURE);
    }
}

void memory_map_df_file(DataFile* df){
    df->write_ptr = mmap(NULL, df->size, PROT_READ | PROT_WRITE, MAP_SHARED, df->fd, 0);
    df->start_ptr = df->write_ptr;
    df->write_ptr = (char*)df->write_ptr + BLOCK_SIZE/16;

    if (df->write_ptr == MAP_FAILED) {
        perror("Error mapping database file");
        exit(EXIT_FAILURE);
    }
}

void set_file_dirty_db(DBFile* db, bool dirty){
    db->dirty = dirty;
}

void set_new_file_free_blocks(DBFile* db){
    db->free_blocks = 0;
}

void init_create_db_memory_block(DBFile* db){
    if (!(~ftruncate(db->fd, INITIAL_DB_SIZE))) {
        perror("Error creating memory block");
        exit(EXIT_FAILURE);
    }
    db->size = INITIAL_DB_SIZE;
}

void init_create_df_memory_block(DataFile* df){
    if (!(~ftruncate(df->fd, INITIAL_DB_SIZE))) {
        perror("Error creating memory block");
        exit(EXIT_FAILURE);
    }
    df->size = INITIAL_DB_SIZE;
}

void create_memory_block(DBFile* db) {
    const size_t new_size = db->size + PAGE_SIZE;
    printf("New size %ld\n", new_size);
    if (ftruncate(db->fd, new_size) == -1) {
        perror("Error extending file size");
        exit(EXIT_FAILURE);
    }

    munmap(db->data, db->size);
    db->data = mmap(NULL, new_size, PROT_READ | PROT_WRITE, MAP_SHARED, db->fd, 0);
    if (db->data == MAP_FAILED) {
        perror("Error mapping expanded database file");
        exit(EXIT_FAILURE);
    }

    db->size = new_size;
}

void write_on_memory_block(DBFile *db, void* new_data, uint64_t page_num){
    printf("%d\n", db->free_blocks);

    while ((page_num+1)*PAGE_SIZE>db->size) {
        create_memory_block(db);
    }

    printf("Block index %ld\n", page_num);

    void* page = (char*)db->data + (page_num * PAGE_SIZE);

    printf("%p\n", page);
    memcpy(page, new_data, PAGE_SIZE);
    db->dirty = 1;
}

void commit_changes_db(DBFile *db) {
    if (!db->dirty) {
        printf("No changes to commit.\n");
        return;
    }

    serialize_btree(db, root);

    if (msync(db->data, db->size, MS_SYNC) == -1) {
        perror("Error committing changes to disk");
        exit(EXIT_FAILURE);
    }

    set_file_dirty_db(db, false);
    printf("Changes successfully committed to disk.\n");
}
