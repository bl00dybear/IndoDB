#include "libraries.h"
#include "config.h"
#include "memory_op.h"
#include "data_structures.h"
#include "row_btree_op.h"


void cli_interactions(){
    bool exit = false;
    while (!exit) {
        printf("Choose an option:\n");
        printf("1. Insert a value\n");
        printf("2. Delete a value\n");
        printf("3. Search for a value\n");
        printf("4. Print the tree\n");
        printf("5. Commit changes\n");
        printf("6. Exit\n");

        int choice;
        scanf("%d", &choice);

        switch (choice) {
            case 1:
                int val;
                printf("Enter a value to insert: ");
                scanf("%d", &val);
                insert(val);
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

    DBFile* db;

    if(!(db = malloc(sizeof(DBFile)))){
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