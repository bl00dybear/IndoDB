#include "data_structures.h"
#include "btree_operations.h"

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

void get_file_size(db_file* db){
    struct stat st;
    if(!(~fstat(db->fd, &st))){
        perror("Error getting file size");
        exit(EXIT_FAILURE);
    }
    // printf("File size %ld\n",st.st_size);
    db->size = st.st_size;
}

void memory_map_file(db_file* db){
    if((db->data = mmap(NULL, db->size, PROT_READ | PROT_WRITE, MAP_SHARED, db->fd, 0)) == MAP_FAILED){
        perror("Error mapping database file");
        exit(EXIT_FAILURE);
    }
}

void set_file_dirty(db_file* db, bool dirty){
    db->dirty = dirty;
}

void set_new_file_free_blocks(db_file* db){
    db->free_blocks = 0;
}

void init_create_memory_block(db_file* db){
    if(!(~ftruncate(db->fd, INITIAL_DB_SIZE))){
        perror("Error creating memory block");
        exit(EXIT_FAILURE);
    }
    db->size = INITIAL_DB_SIZE;

}

void create_memory_block(db_file* db) {
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

void write_on_memory_block(db_file *db, void* new_data){
    int block_index;
    printf("%ld\n",db->free_blocks);
    if(!db->free_blocks){
        create_memory_block(db);
        block_index = (db->size/PAGE_SIZE) - 1;
    }
    else{
        //implement queue tp store free blocks
    }  
    printf("Block index %d\n",block_index);

    void* page = (char*)db->data + (block_index * PAGE_SIZE);

    printf("%x\n",page);
    memcpy(page, new_data, PAGE_SIZE);
    db->dirty = 1;
}
























void cli_interactions(db_file* db){
    bool exit = false;
    while(!exit){
        printf("Choose an option:\n");
    printf("1. Insert a value\n");
    printf("2. Delete a value\n");
    printf("3. Search for a value\n");
    printf("4. Print the tree\n");
    printf("5. Commit changes\n");
    printf("6. Exit\n");

    int choice;
    scanf("%d", &choice);

    switch(choice){
        case 1:
            printf("Enter a value to insert: ");
            int val;
            scanf("%d", &val);
            insert(val);
            char new_data[PAGE_SIZE];
            memset(new_data, (char)(val + '0'), PAGE_SIZE);
            // printf("%s",new_data);
            write_on_memory_block(db, new_data);
            break;
        case 2:
            printf("Enter a value to delete: ");
            int del;
            scanf("%d", &del);
            delete_value_from_tree(del);
            break;
        case 3:
            printf("Enter a value to search: ");
            int search_val;
            scanf("%d", &search_val);
            int pos;
            search(search_val, &pos, root);
            break;
        case 4:
            traversal(root);
            printf("\n");
            break;
        case 5:
            // Commit changes
            break;
        case 6:
            exit = true;
            break;
        default:
            printf("Invalid choice\n");
    }
    }


    


}



int main(){
    // Step 1: Verify if the file exists in directory
    db_file* db;
    if(!(db = malloc(sizeof(db_file)))){
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!(~access(DB_FILENAME, F_OK)))
        create_database_file(&db->fd);

    open_database_file(&db->fd);
    get_file_size(db);
    // printf("File size %ld\n",db->size);

    if(!db->size)
        init_create_memory_block(db);

    printf("File size %ld\n",db->size);

    memory_map_file(db);

    set_file_dirty(db, false);
    set_new_file_free_blocks(db);

    printf("%d %ld\n",db->fd,db->size);

    cli_interactions(db);

    free(db);
    exit(EXIT_SUCCESS);
}