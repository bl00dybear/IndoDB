#include "../headers/memory_op.h"
#include "../headers/libraries.h"
#include "../headers/config.h"
#include "../headers/data_structures.h"

void create_database_file(){
    if (!(~creat(DB_FILENAME, 0644))) {
        perror("Error creating database file");
        exit(EXIT_FAILURE);
    }
}

void open_database_file(int* db_filedescriptor){
    if (!(~((*db_filedescriptor) = open(DB_FILENAME, O_RDWR | O_CREAT, 0644)))) {
        perror("Error opening database file");
        exit(EXIT_FAILURE);
    }
}

void get_file_size(DBFile* db){
    struct stat st;
    if (!(~fstat(db->fd, &st))) {
        perror("Error getting file size");
        exit(EXIT_FAILURE);
    }
    db->size = st.st_size;
}

void memory_map_file(DBFile* db){
    db->data = mmap(NULL, db->size, PROT_READ | PROT_WRITE, MAP_SHARED, db->fd, 0);
    if (db->data == MAP_FAILED) {
        perror("Error mapping database file");
        exit(EXIT_FAILURE);
    }
}

void set_file_dirty(DBFile* db, bool dirty){
    db->dirty = dirty;
}

void set_new_file_free_blocks(DBFile* db){
    db->free_blocks = 0;
}

void init_create_memory_block(DBFile* db){
    if (!(~ftruncate(db->fd, INITIAL_DB_SIZE))) {
        perror("Error creating memory block");
        exit(EXIT_FAILURE);
    }
    db->size = INITIAL_DB_SIZE;
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

void write_on_memory_block(DBFile *db, const void* new_data, u_int64_t page_num){
    printf("%d\n", db->free_blocks);
    if (!db->free_blocks) {
        create_memory_block(db);
    } else {
        // TODO: implement queue to store free blocks
    }

    printf("Block index %ld\n", page_num);

    void* page = (char*)db->data + (page_num * PAGE_SIZE);

    printf("%p\n", page);
    memcpy(page, new_data, PAGE_SIZE);
    db->dirty = 1;
}

void commit_changes(DBFile *db) {
    if (!db->dirty) {
        printf("No changes to commit.\n");
        return;
    }

    serialize_btree(db, root);

    if (msync(db->data, db->size, MS_SYNC) == -1) {
        perror("Error committing changes to disk");
        exit(EXIT_FAILURE);
    }

    set_file_dirty(db, false);
    printf("Changes successfully committed to disk.\n");
}
