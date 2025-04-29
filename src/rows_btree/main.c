#include "headers/libraries.h"
#include "headers/config.h"
#include "headers/row_btree_op.h"
#include "headers/memory_op.h"
#include "headers/data_structures.h"
#include "headers/queue.h"
#include <stdio.h>


DBFile* db;

void cli_interactions(){
    bool exit = false;
    
    while (!exit) {
        printf("Choose an option:\n");
        printf("1. Insert a value\n");
        printf("2. Delete a value\n");
        printf("3. Search for a value\n");
        printf("4. Print the tree\n");
        printf("5. Commit changes\n");
        printf("6. Load database\n");
        printf("7. Exit\n");

        int choice;
        scanf("%d", &choice);

        switch (choice) {
            case 1:
                int val;
                printf("Enter a value to insert: ");
                // scanf("%d", &val);
                // insert(val);
                for(int i=0;i<250;i++)
                {
                    insert(i);
                }
                set_file_dirty(db,true);
                break;
                case 2:
                printf("Enter a value to delete: ");
                int del;
                scanf("%d", &del);
                delete_value_from_tree(del);
                set_file_dirty(db,true);
                break;
            case 3:
                printf("Enter a value to search: ");
                int search_val;
                scanf("%d", &search_val);
                int pos;
                search(search_val, &pos, root);
                break;
            case 4:
                printf("1\n");
                printf("Root nums  %ld, %ld, %ld\n", root->keys[1],root->keys[2],root->keys[3]);
                printf("2\n");
                traversal(root);
                printf("\n");
                RowNode *temp = root;
                break;
            case 5:
                commit_changes(db);
                break;
            case 6:
                root = load_btree_from_disk(db);
                printf("%ld\n",root->page_num);
                if (root) {
                    printf("B-Tree successfully loaded!\n");
                } else {
                    printf("Failed to load B-Tree.\n");
                }
                break;
            case 7:
                exit = true;
                break;

            default:
                printf("Invalid choice\n");
        }
    }
}

int main(){

    if(!(db = malloc(sizeof(DBFile)))){
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!(~access(DB_FILENAME, F_OK)))
        create_database_file(&db->fd);

    open_database_file(&db->fd);
    get_file_size(db);

    if(!db->size)
        init_create_memory_block(db);

    printf("File size %ld\n",db->size);

    memory_map_file(db);

    set_file_dirty(db, false);
    set_new_file_free_blocks(db);

    printf("%d %ld\n",db->fd,db->size);

    free_page_queue = create_queue();
  
    for(int i=1;i<=100000;i+=1){
        push(free_page_queue,i);
    }

    cli_interactions();

    free(db);
    
    exit(EXIT_SUCCESS);
}