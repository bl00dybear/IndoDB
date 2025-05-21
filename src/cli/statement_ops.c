#include "../include/cli/statement_ops.h"

void process_statement(Statement *stmt) {

    switch (stmt->type) {
        case STATEMENT_INSERT: {
            if (strcmp(DB_FILENAME, "../databases") == 0 || strcmp(DB_FILENAME, "../databases/") == 0) {
                printf("Error: No database selected. Use 'USE DATABASE db_name' first.\n");
                break;
            }
            database_init(stmt->insertStmt.table);
            if (!metadata) {
                printf("Error: Metadata initialization failed\n");
                break;
            }
            deserialize_metadata(db, metadata);

            for(int i = 0; i < metadata->num_columns; i++) {
                
            }

            // printf("metadata->table_name: %s\n", metadata->table_name);
            // printf("metadata ->num_columns: %d\n", metadata->num_columns);
            // printf("metadata ->root_page_num: %ld\n", metadata->root_page_num);

            if(!strcmp(metadata->table_name,stmt->insertStmt.table)) {
                uint64_t row_size = 0;
                char* row_content = get_row_content(stmt,&row_size);
                // printf("Row content: %ld\n",row_size);
                void *written_address = write_row(df, row_content, row_size);
                const uint64_t current_id = global_id++;
                insert(current_id,written_address);
                set_file_dirty_db(db, true);
                set_file_dirty_df(df,true);


                commit_changes_db(db, metadata);
                commit_changes_df(df);
            }else{
                printf("Error: Table %s not found in database\n", stmt->insertStmt.table);
            }

            break;
        }
        case STATEMENT_SELECT: {
            if (strcmp(DB_FILENAME, "../databases") == 0 || strcmp(DB_FILENAME, "../databases/") == 0) {
                printf("Error: No database selected. Use 'USE DATABASE db_name' first.\n");
                break;
            }
            db=NULL;
            df=NULL;
            metadata=NULL;
            free_page_queue=NULL;
            char dbfilepath[256] = {0};
            strcpy(dbfilepath, DB_FILENAME);
            strcat(dbfilepath, stmt->selectStmt.table); 
            strcat(dbfilepath, ".bin");
            // printf("%s\n\n", dbfilepath);

            char datafilepath[256] = {0};
            strcpy(datafilepath, DATA_FILENAME);
            strcat(datafilepath, stmt->selectStmt.table);
            strcat(datafilepath, ".bin");

            if(!(~access(dbfilepath, F_OK)))
            {   
                perror("Error: Database file not found"); 
                break;
            }
            
            if(!(~access(datafilepath, F_OK)))
            {
                perror("Error: Data file not found");
                break;
            }
            database_init(stmt->selectStmt.table);
            if(!strcmp(metadata->table_name,stmt->selectStmt.table)) {
                if (!strcmp(stmt -> selectStmt.columns[0],"*")) {
                   char *column_pointers[MAX_COLUMNS];
                    for (int i = 0; i < metadata->num_columns; i++) {
                        column_pointers[i] = metadata->column_names[i];
                    }

                    display_table(column_pointers, metadata->num_columns, metadata, stmt);
                } else {
                    display_table(stmt->selectStmt.columns, stmt->selectStmt.num_columns, metadata, stmt);
                }
            } else {
                    printf("Error: Table %s is empty\n", stmt->selectStmt.table);
            }

            break;
        }
        case STATEMENT_CREATE: {
            if (strcmp(DB_FILENAME, "../databases") == 0 || strcmp(DB_FILENAME, "../databases/") == 0) {
                printf("Error: No database selected. Use 'USE DATABASE db_name' first.\n");
                break;
            }
            char dbfilepath[256] = {0};
            strcpy(dbfilepath, DB_FILENAME);
            strcat(dbfilepath, stmt->createStmt.table); 
            strcat(dbfilepath, ".bin");
            if(access(dbfilepath, F_OK) == 0) {
                printf("Error: Table '%s' already exists.\n", stmt->createStmt.table);
                break;
            }
            

            database_init(stmt->createStmt.table);
            printf("Table %s created successfully!\n", stmt->createStmt.table);

            set_table_parameters(metadata, stmt);

            serialize_metadata(db, metadata);
            set_file_dirty_db(db, true);
            commit_changes_db(db, metadata);
                      

            break;
        }
        case STATEMENT_DROP: {
            if (strcmp(DB_FILENAME, "../databases") == 0 || strcmp(DB_FILENAME, "../databases/") == 0) {
                printf("Error: No database selected. Use 'USE DATABASE db_name' first.\n");
                break;
            }
            char filepath_db[256] = {0};
            char filepath_df[256] = {0};
            
            // Build database and datafile paths
            strcpy(filepath_db, DB_FILENAME);
            strcat(filepath_db, stmt->dropStmt.table);
            strcat(filepath_db, ".bin");
            
            strcpy(filepath_df, DATA_FILENAME);
            strcat(filepath_df, stmt->dropStmt.table);
            strcat(filepath_df, ".bin");
            
            // Check if the files exist before attempting to delete
            if (access(filepath_db, F_OK) != 0 || access(filepath_df, F_OK) != 0) {
                printf("Error: Table '%s' does not exist.\n", stmt->dropStmt.table);
                break;
            }
            
            // Free current resources if they're loaded
            if (db) {
                if (db->data) {
                    munmap(db->data, db->size);
                }
                close(db->fd);
                free(db);
                db = NULL;
            }
            
            if (df) {
                if (df->start_ptr) {
                    munmap(df->start_ptr, df->size);
                }
                close(df->fd);
                free(df);
                df = NULL;
            }
            
            if (metadata) {
                free(metadata);
                metadata = NULL;
            }
            
            if (free_page_queue) {
                destroy_queue(free_page_queue);
                free_page_queue = NULL;
            }
            
            // Reset global values
            root = NULL;
            visited_count = 0;
            serialized_count = 0;
            global_id = 1;
            
            // Try to remove the files
            int result_db = remove(filepath_db);
            int result_df = remove(filepath_df);
            
            if (result_db == 0 && result_df == 0) {
                printf("Table '%s' dropped successfully.\n", stmt->dropStmt.table);
            } else {
                printf("Error dropping table '%s'. ", stmt->dropStmt.table);
                if (result_db != 0) {
                    printf("Error: Database file error\n");
                }
                if (result_df != 0) {
                    printf("Error: Data file error\n");
                }
                printf("\n");
            }
            
            break;
        }
        case STATEMENT_CREATE_DB: {
            if (strcmp(DB_FILENAME, "../databases") != 0 && strcmp(DB_FILENAME, "../databases/") != 0) {
                printf("Error: Cannot create database inside another database.\n");
                printf("You are currently in database: %s\n", strrchr(DB_FILENAME, '/') + 1);
                break;
            }
            char dbfilepath[256] = {0};
            strcpy(dbfilepath, DB_FILENAME);
            strcat(dbfilepath, "/");
            strcat(dbfilepath, stmt->createDbStmt.database);

            if(access(dbfilepath, F_OK) == 0) {
                printf("Error: Database '%s' already exists.\n", stmt->createDbStmt.database);
                break;
            }
            if(mkdir(dbfilepath, 0777) == -1) {
                perror("Error creating database directory");
                break;
            }

            
            break;
        }
        case STATEMENT_DROP_DB: {
            if (strcmp(DB_FILENAME, "../databases") != 0 && strcmp(DB_FILENAME, "../databases/") != 0) {
                printf("Error: Drop database is only available outside any database.\n Use 'CLOSE DATABASE' first.\n");
                break;
            }
            char dbfilepath[256] = {0};
            strcpy(dbfilepath, DB_FILENAME);
            strcat(dbfilepath, "/");
            strcat(dbfilepath, stmt->dropDbStmt.database);

            // Verifică dacă directorul există
            if(access(dbfilepath, F_OK) != 0) {
                printf("Error: Database '%s' does not exist.\n", stmt->dropDbStmt.database);
                break;
            }
            
            // Șterge directorul și conținutul său recursiv
            char rm_command[512];
            snprintf(rm_command, sizeof(rm_command), "rm -rf \"%s\"", dbfilepath);
            
            if(system(rm_command) != 0) {
                perror("Error deleting database directory");
                break;
            }
            
            printf("Database '%s' dropped successfully!\n", stmt->dropDbStmt.database);
            break;
        }
        case STATEMENT_USE_DB: {
            if (strcmp(DB_FILENAME, "../databases") != 0 && strcmp(DB_FILENAME, "../databases/") != 0){
                
                printf("Use database is only available outside any database.\n Use 'CLOSE DATABASE' first.\n");
                printf("You are currently in database: %s\n", strrchr(DB_FILENAME, '/') + 1);
                break;}
            char dbfilepath[256] = {0};
            strcpy(dbfilepath, "../databases/");
            strcat(dbfilepath, stmt->useDbStmt.database);

            // Verifică dacă directorul există
            if(access(dbfilepath, F_OK) != 0) {
                printf("Error: Database '%s' does not exist.\n", stmt->useDbStmt.database);
                break;
            }
            
            // Actualizează variabilele globale de cale
            strcpy(DB_FILENAME, "../databases/");
            strcat(DB_FILENAME, stmt->useDbStmt.database);
            
            strcpy(DATA_FILENAME, "../databases/");
            strcat(DATA_FILENAME, stmt->useDbStmt.database);
            
            printf("Database changed to '%s'\n", stmt->useDbStmt.database);
            break;
        }
        default: break;
    }
}


void free_memory(Statement* stmt) {
    free_statement(stmt);

    if (free_page_queue) {
        destroy_queue(free_page_queue);
        free_page_queue = NULL;
    }

    if (db) {
        close(db->fd);
        free(db);
        db = NULL;
    }

    if (df) {
        close(df->fd);
        free(df);
        df = NULL;
    }

    root = NULL;
    visited_count = 0;
    serialized_count = 0;
    global_id = 1;
}

void free_statement(Statement *stmt) {
    if (stmt->type == STATEMENT_INSERT) {
        for (int i = 0; i < stmt->insertStmt.num_columns; i++) {
            free(stmt->insertStmt.columns[i]);
        }
        free(stmt->insertStmt.columns);

        for (int i = 0; i < stmt->insertStmt.num_values; i++) {
            free(stmt->insertStmt.values[i].value);
            free(stmt->insertStmt.values[i].valueType);
        }
        free(stmt->insertStmt.values);

        free(stmt->insertStmt.table);
    } else if (stmt->type == STATEMENT_SELECT) {
        for (int i = 0; i < stmt->selectStmt.num_columns; i++) {
            free(stmt->selectStmt.columns[i]);
        }
        free(stmt->selectStmt.columns);
        free(stmt->selectStmt.table);
        if (stmt->selectStmt.condition) {
            free(stmt->selectStmt.condition);
        }
    }
}


void verify_constraints(Statement *stmt, MetadataPage *metadata) {
    for(int i = 0; i < metadata->num_columns; i++) {
        if (metadata->column_constraints[i] == CONSTRAINT_NOT_NULL) {
            // Check if the value is NULL
            if (stmt->insertStmt.values[i].value == NULL) {
                printf("Error: Column %s cannot be NULL\n", metadata->column_names[i]);
                return;
            }
        } else if (metadata->column_constraints[i] == CONSTRAINT_UNIQUE) {
            
        } else if (metadata->column_constraints[i] == CONSTRAINT_PRIMARY_KEY) {
            // Check for primary key constraints
            // This is a placeholder, implement your own logic to check for primary key constraints
        }
    }
    // Implement constraint verification logic here
    // For example, check if the values meet the constraints defined in the metadata
    // This is a placeholder function and should be implemented based on your requirements
}