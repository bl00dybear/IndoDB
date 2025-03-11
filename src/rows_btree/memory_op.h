#ifndef DA9FE760_8A8C_4090_BD62_6DAF91BCFDBA
#define DA9FE760_8A8C_4090_BD62_6DAF91BCFDBA

#include "libraries.h"
#include "data_structures.h"
#include "config.h"

void create_database_file(){
    if(!(~creat(DB_FILENAME, 0644))){
        perror("Error creating database file");
        exit(EXIT_FAILURE);
    }
}

void open_database_file(int* db_filedescriptor){
    if(!(~((*db_filedescriptor) = open(DB_FILENAME, O_RDWR | O_CREAT, 0644)))){
        perror("Error opening database file");
        exit(EXIT_FAILURE);
    }
}

void get_file_size(DBFile* db){
    struct stat st;
    if(!(~fstat(db->fd, &st))){
        perror("Error getting file size");
        exit(EXIT_FAILURE);
    }
    // printf("File size %ld\n",st.st_size);
    db->size = st.st_size;
}

void memory_map_file(DBFile* db){
    if((db->data = mmap(NULL, db->size, PROT_READ | PROT_WRITE, MAP_SHARED, db->fd, 0)) == MAP_FAILED){
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
    if(!(~ftruncate(db->fd, INITIAL_DB_SIZE))){
        perror("Error creating memory block");
        exit(EXIT_FAILURE);
    }
    db->size = INITIAL_DB_SIZE;

}

void create_memory_block(DBFile* db) {
    size_t new_size = db->size + PAGE_SIZE;
    printf("New size %ld\n",new_size);
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

    // db->data = new_data;
    db->size = new_size;
}

void write_on_memory_block(DBFile *db, void* new_data){
    int block_index;
    printf("%d\n",db->free_blocks);
    if(!db->free_blocks){
        create_memory_block(db);
        block_index = (db->size/PAGE_SIZE) - 1;
    }
    else{
        //implement queue tp store free blocks
    }  
    printf("Block index %d\n",block_index);

    void* page = (char*)db->data + (block_index * PAGE_SIZE);

    printf("%p\n",page);
    memcpy(page, new_data, PAGE_SIZE);
    db->dirty = 1;
}

#endif /* DA9FE760_8A8C_4090_BD62_6DAF91BCFDBA */
