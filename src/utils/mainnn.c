// #include "../../include/libraries.h"
// #include "../../include/config.h"
// #include "../../include/core/row_btree_ops.h"
// #include "../../include/core/memory_ops.h"
// #include "../../include/data/core_structures.h"
// #include "../../include/utils/queue.h"
// #include "../../include/core/datafile_ops.h"
// #include <stdio.h>
//
//
// DBFile* db;
// DataFile* df;
// uint64_t global_id = 1;
//
// void cli_interactions(){
//     bool exit = false;
//
//     while (!exit) {
//         printf("Choose an option:\n");
//         printf("1. Insert a value\n");
//         printf("2. Delete a value\n");
//         printf("3. Search for a value\n");
//         printf("4. Print the tree\n");
//         printf("5. Commit changes\n");
//         printf("6. Load database\n");
//         printf("7. Exit\n");
//
//         int choice;
//         if (scanf("%d", &choice) != 1) {
//             int c;
//             while ((c = getchar()) != '\n' && c != EOF)
//                 ;
//
//             clearerr(stdin);
//
//             printf("Invalid input. Please enter a number.\n");
//             continue;
//         }
//
//         int c;
//         while ((c = getchar()) != '\n' && c != EOF)
//             ;
//
//         switch (choice) {
//             case 1: {
//                 char buffer[102400];
//                 size_t total_len = 0;
//                 const size_t capacity = sizeof(buffer) - 1;
//
//                 printf("Enter text to write (press Enter followed by Ctrl+D to finish):\n");
//
//                 while (total_len < capacity) {
//                     c = getchar();
//                     if (c == EOF) {
//                         clearerr(stdin);
//                         break;
//                     }
//                     buffer[total_len++] = (char)c;
//                 }
//
//                 total_len-=1;
//                 buffer[total_len] = '\0';
//
//                 if (total_len > 0) {
//                     void *written_address = write_row(df, buffer, total_len + 1);
//                     printf("Text written at address: %p\n", written_address);
//                     printf("Total characters written: %zu\n", total_len);
//
//                     const uint64_t current_id = global_id++;
//                     insert(current_id,written_address);
//                     printf("Inserted record with ID: %lu\n", current_id);
//                     printf("Record written at address: %p\n", written_address);
//
//                     set_file_dirty_db(db, true);
//                     set_file_dirty_df(df,true);
//                 }
//
//
//                 break;
//             }
//             case 2: {
//                 printf("Enter a value to delete: ");
//                 int del;
//                 if (scanf("%d", &del) != 1) {
//                     printf("Invalid input\n");
//                     while ((c = getchar()) != '\n' && c != EOF)
//                         ;
//                     clearerr(stdin);
//                     break;
//                 }
//                 delete_value_from_tree(del);
//                 set_file_dirty_db(db, true);
//                 set_file_dirty_df(df, true);
//                 break;
//             }
//             case 3: {
//                 printf("Enter a value to search: ");
//                 int search_val;
//                 if (scanf("%d", &search_val) != 1) {
//                     printf("Invalid input\n");
//                     while ((c = getchar()) != '\n' && c != EOF);
//                     clearerr(stdin);
//                     break;
//                 }
//                 int pos;
//                 search(search_val, &pos, root);
//                 break;
//             }
//             case 4:
//                 printf("1\n");
//                 printf("Root nums  %ld, %ld, %ld\n", root->keys[1], root->keys[2], root->keys[3]);
//                 printf("2\n");
//                 traversal(root);
//                 printf("\n");
//                 break;
//             case 5:
//                 commit_changes_db(db);
//                 commit_changes_df(df);
//                 break;
//             case 6:
//                 root = load_btree_from_disk(db);
//                 printf("%ld\n", root->page_num);
//                 if (root) {
//                     printf("B-Tree successfully loaded!\n");
//                 } else {
//                     printf("Failed to load B-Tree.\n");
//                 }
//
//                 load_datafile(df);
//                 break;
//             case 7:
//                 exit = true;
//                 break;
//             default:
//                 printf("Invalid choice\n");
//                 break;
//         }
//     }
// }
//
// int mainn(){
//
//     if(!((db = malloc(sizeof(DBFile))))){
//         perror("Memory allocation failed");
//         exit(EXIT_FAILURE);
//     }
//
//     if(!((df = malloc(sizeof(DataFile))))){
//         perror("Memory allocation failed");
//         exit(EXIT_FAILURE);
//     }
//
//     if(!(~access(DB_FILENAME, F_OK)))
//         create_database_file(DB_FILENAME);
//
//     if(!(~access(DATA_FILENAME, F_OK)))
//         create_database_file(DATA_FILENAME);
//
//     open_database_file(&db->fd,DB_FILENAME);
//     open_database_file(&df->fd,DATA_FILENAME);
//
//     get_db_file_size(db);
//     get_df_file_size(df);
//
//     if(!db->size)
//         init_create_db_memory_block(db);
//
//     if(!df->size)
//         init_create_df_memory_block(df);
//
//     printf("File size %ld\n",db->size);
//
//     memory_map_db_file(db);
//     memory_map_df_file(df);
//
//     set_file_dirty_db(db, false);
//     set_file_dirty_df(df, false);
//     set_new_file_free_blocks(db);
//
//     printf("%d %ld\n",db->fd,db->size);
//
//     free_page_queue = create_queue();
//
//     for(int i=1;i<=10000;i+=1){
//         push(free_page_queue,i);
//     }
//
//     cli_interactions();
//
//     free(db);
//
//     exit(EXIT_SUCCESS);
// }