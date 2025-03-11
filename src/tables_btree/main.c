#include "libraries.h"
#include "config.h"
#include "data_structures.h"
#include "table_btree_op.h"
#include "memory_op.h"
#include <sys/types.h>







void cli_interactions(DBFile* db){
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
    u_int16_t rootPage;
    char tableName[TABLE_NAME_LENGTH];
    switch(choice){
        case 1:
                printf("Enter table name: ");
                scanf("%s", tableName);
                printf("Enter root page number: ");
                scanf("%hu", &rootPage);
                insert(root, tableName, rootPage);
                printf("Inserted '%s'\n", tableName);
                break;
            case 2:
                printf("Delete operation is not implemented yet.\n");
                break;
            case 3:
                printf("Enter table name to search: ");
                scanf("%s", tableName);
                printf("%s\n", search(root, tableName) ? "Found" : "Not Found");
                break;
            case 4:
                printf("B-Tree Traversal:\n");
                traversal(root);
                printf("\n");
                break;
            case 5:
                printf("Commit operation is not implemented yet.\n");
                break;
            case 6:
                exit = true;
                break;
            default:
                printf("Invalid choice\n");
    }
    }


    


}

int main(void){

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
