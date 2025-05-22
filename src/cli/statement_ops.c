#include "../include/cli/statement_ops.h"

void preprocess_insert_statement(Statement *stmt, MetadataPage *metadata) {
    if (!stmt || !metadata || stmt->type != STATEMENT_INSERT) {
        printf("Error: Invalid statement or metadata for preprocessing\n");
        return;
    }
    struct {
        char *value;
        char *valueType;
    } *complete_values = malloc(sizeof(*complete_values) * metadata->num_columns);
    
    if (!complete_values) {
        perror("Failed to allocate memory for insert preprocessing");
        return;
    }
    
    for (uint32_t i = 0; i < metadata->num_columns; i++) {
        switch (metadata->column_types[i]) {
            case TYPE_INT:
                complete_values[i].value = strdup("0");  
                complete_values[i].valueType = strdup("Int");
                break;
            case TYPE_FLOAT:
                complete_values[i].value = strdup("0.0");  
                complete_values[i].valueType = strdup("FLOAT");
                break;
            case TYPE_VARCHAR:
                complete_values[i].value = strdup("NULL");  
                complete_values[i].valueType = strdup("String");
                break;
            case TYPE_TIMESTAMP:
                complete_values[i].value = strdup("0");  
                complete_values[i].valueType = strdup("TIMESTAMP");
                break;
            default:
                complete_values[i].value = strdup("NULL");  
                complete_values[i].valueType = strdup("String");
                break;
        }
    }
    
    for (int i = 0; i < stmt->insertStmt.num_columns; i++) {
        bool column_found = false;
        
        for (uint32_t j = 0; j < metadata->num_columns; j++) {
            if (strcmp(stmt->insertStmt.columns[i], metadata->column_names[j]) == 0) {
                free(complete_values[j].value);
                free(complete_values[j].valueType);
                
                if (stmt->insertStmt.values[i].value) {
                    complete_values[j].value = strdup(stmt->insertStmt.values[i].value);
                } else {
                    switch (metadata->column_types[j]) {
                        case TYPE_INT:
                            complete_values[j].value = strdup("0");
                            break;
                        case TYPE_FLOAT:
                            complete_values[j].value = strdup("0.0");
                            break;
                        case TYPE_VARCHAR:
                            complete_values[j].value = strdup("NULL");
                            break;
                        case TYPE_TIMESTAMP:
                            complete_values[j].value = strdup("0");
                            break;
                        default:
                            complete_values[j].value = strdup("NULL");
                            break;
                    }
                }
                
                complete_values[j].valueType = strdup(stmt->insertStmt.values[i].valueType);
                
                column_found = true;
                break;
            }
        }
        
        if (!column_found) {
            printf("Warning: Column '%s' does not exist in table '%s'\n", 
                  stmt->insertStmt.columns[i], metadata->table_name);
        }
    }
    
    for (int i = 0; i < stmt->insertStmt.num_values; i++) {
        free(stmt->insertStmt.values[i].value);
        free(stmt->insertStmt.values[i].valueType);
    }
    free(stmt->insertStmt.values);
    
    for (int i = 0; i < stmt->insertStmt.num_columns; i++) {
        free(stmt->insertStmt.columns[i]);
    }
    free(stmt->insertStmt.columns);
    
    stmt->insertStmt.values = complete_values;
    stmt->insertStmt.num_values = metadata->num_columns;
    
    stmt->insertStmt.columns = malloc(sizeof(char*) * metadata->num_columns);
    if (!stmt->insertStmt.columns) {
        perror("Failed to allocate memory for column names");
        return;
    }
    
    for (uint32_t i = 0; i < metadata->num_columns; i++) {
        stmt->insertStmt.columns[i] = strdup(metadata->column_names[i]);
    }
    stmt->insertStmt.num_columns = metadata->num_columns;
}

void replace_null_values(Statement *stmt, MetadataPage *metadata) {
    if (!stmt || !metadata || stmt->type != STATEMENT_INSERT) {
        printf("Error: Invalid statement or metadata for NULL value replacement\n");
        return;
    }
    
    // Structura temporară pentru a stoca valorile de insert complete
    struct {
        char *value;
        char *valueType;
    } *complete_values = malloc(sizeof(*complete_values) * metadata->num_columns);
    
    if (!complete_values) {
        perror("Failed to allocate memory for NULL value replacement");
        return;
    }
    
    // Inițializează toate valorile cu valori implicite pentru NULL în funcție de tipul coloanei
    for (uint32_t i = 0; i < metadata->num_columns; i++) {
        switch (metadata->column_types[i]) {
            case TYPE_INT:
                complete_values[i].value = strdup("0");
                complete_values[i].valueType = strdup("Int");
                break;
            case TYPE_FLOAT:
                complete_values[i].value = strdup("0.0");
                complete_values[i].valueType = strdup("FLOAT");
                break;
            case TYPE_VARCHAR:
                complete_values[i].value = strdup("NULL");
                complete_values[i].valueType = strdup("String");
                break;
            case TYPE_TIMESTAMP:
                complete_values[i].value = strdup("0");
                complete_values[i].valueType = strdup("TIMESTAMP");
                break;
            default:
                complete_values[i].value = strdup("NULL");
                complete_values[i].valueType = strdup("String");
                break;
        }
    }
    
    // Cazul când nu avem coloane specificate, dar avem valori
    if (stmt->insertStmt.columns == NULL || stmt->insertStmt.num_columns == 0) {
        // Verificăm dacă avem numărul corect de valori
        if (stmt->insertStmt.num_values != metadata->num_columns) {
            printf("Error: Number of values (%d) doesn't match number of columns (%d) in table\n",
                   stmt->insertStmt.num_values, metadata->num_columns);
            
            // Eliberăm memoria alocată
            for (uint32_t i = 0; i < metadata->num_columns; i++) {
                free(complete_values[i].value);
                free(complete_values[i].valueType);
            }
            free(complete_values);
            return;
        }
        
        // Copiem valorile direct, fără a căuta coloanele
        for (uint32_t i = 0; i < metadata->num_columns && i < (uint32_t)stmt->insertStmt.num_values; i++) {
            // Eliberăm valorile implicite
            free(complete_values[i].value);
            free(complete_values[i].valueType);
            
            // Verificăm dacă valoarea este NULL
            if (!stmt->insertStmt.values[i].value || 
                strcmp(stmt->insertStmt.values[i].value, "NULL") == 0) {
                // Folosim valoarea implicită pentru tipul acestei coloane
                switch (metadata->column_types[i]) {
                    case TYPE_INT:
                        complete_values[i].value = strdup("0");
                        complete_values[i].valueType = strdup("Int");
                        break;
                    case TYPE_FLOAT:
                        complete_values[i].value = strdup("0.0");
                        complete_values[i].valueType = strdup("FLOAT");
                        break;
                    case TYPE_VARCHAR:
                        complete_values[i].value = strdup("NULL");
                        complete_values[i].valueType = strdup("String");
                        break;
                    case TYPE_TIMESTAMP:
                        complete_values[i].value = strdup("0");
                        complete_values[i].valueType = strdup("TIMESTAMP");
                        break;
                    default:
                        complete_values[i].value = strdup("NULL");
                        complete_values[i].valueType = strdup("String");
                        break;
                }
            } else {
                // Copiază valoarea direct
                complete_values[i].value = strdup(stmt->insertStmt.values[i].value);
                
                // Asigură-te că valueType corespunde tipului coloanei
                switch (metadata->column_types[i]) {
                    case TYPE_INT:
                        complete_values[i].valueType = strdup("Int");
                        break;
                    case TYPE_FLOAT:
                        complete_values[i].valueType = strdup("FLOAT");
                        break;
                    case TYPE_VARCHAR:
                        complete_values[i].valueType = strdup("String");
                        break;
                    case TYPE_TIMESTAMP:
                        complete_values[i].valueType = strdup("TIMESTAMP");
                        break;
                    default:
                        complete_values[i].valueType = strdup("String");
                        break;
                }
            }
        }
    } else {
        // Cazul când coloanele sunt specificate - trebuie să le mapăm la pozițiile corecte
        for (int i = 0; i < stmt->insertStmt.num_columns; i++) {
            bool column_found = false;
            
            for (uint32_t j = 0; j < metadata->num_columns; j++) {
                if (strcmp(stmt->insertStmt.columns[i], metadata->column_names[j]) == 0) {
                    // Eliberăm valorile implicite
                    free(complete_values[j].value);
                    free(complete_values[j].valueType);
                    
                    // Verificăm dacă valoarea este NULL
                    if (!stmt->insertStmt.values[i].value || 
                        strcmp(stmt->insertStmt.values[i].value, "NULL") == 0) {
                        // Folosim valoarea implicită pentru tipul acestei coloane
                        switch (metadata->column_types[j]) {
                            case TYPE_INT:
                                complete_values[j].value = strdup("0");
                                complete_values[j].valueType = strdup("Int");
                                break;
                            case TYPE_FLOAT:
                                complete_values[j].value = strdup("0.0");
                                complete_values[j].valueType = strdup("FLOAT");
                                break;
                            case TYPE_VARCHAR:
                                complete_values[j].value = strdup("NULL");
                                complete_values[j].valueType = strdup("String");
                                break;
                            case TYPE_TIMESTAMP:
                                complete_values[j].value = strdup("0");
                                complete_values[j].valueType = strdup("TIMESTAMP");
                                break;
                            default:
                                complete_values[j].value = strdup("NULL");
                                complete_values[j].valueType = strdup("String");
                                break;
                        }
                    } else {
                        // Copiază valoarea direct
                        complete_values[j].value = strdup(stmt->insertStmt.values[i].value);
                        
                        // Asigură-te că valueType corespunde tipului coloanei
                        switch (metadata->column_types[j]) {
                            case TYPE_INT:
                                complete_values[j].valueType = strdup("Int");
                                break;
                            case TYPE_FLOAT:
                                complete_values[j].valueType = strdup("FLOAT");
                                break;
                            case TYPE_VARCHAR:
                                complete_values[j].valueType = strdup("String");
                                break;
                            case TYPE_TIMESTAMP:
                                complete_values[j].valueType = strdup("TIMESTAMP");
                                break;
                            default:
                                complete_values[j].valueType = strdup("String");
                                break;
                        }
                    }
                    
                    column_found = true;
                    break;
                }
            }
            
            if (!column_found) {
                printf("Warning: Column '%s' does not exist in table '%s'\n", 
                      stmt->insertStmt.columns[i], metadata->table_name);
            }
        }
    }
    
    // Eliberăm vechea structură de valori
    for (int i = 0; i < stmt->insertStmt.num_values; i++) {
        free(stmt->insertStmt.values[i].value);
        free(stmt->insertStmt.values[i].valueType);
    }
    free(stmt->insertStmt.values);
    
    // Eliberăm și vechiul array de coloane dacă există
    for (int i = 0; i < stmt->insertStmt.num_columns; i++) {
        free(stmt->insertStmt.columns[i]);
    }
    free(stmt->insertStmt.columns);
    
    // Alocă noul array de coloane
    stmt->insertStmt.columns = malloc(sizeof(char*) * metadata->num_columns);
    if (!stmt->insertStmt.columns) {
        perror("Failed to allocate memory for column names");
        
        // Eliberăm complete_values dacă alocarea eșuează
        for (uint32_t i = 0; i < metadata->num_columns; i++) {
            free(complete_values[i].value);
            free(complete_values[i].valueType);
        }
        free(complete_values);
        return;
    }
    
    // Copiază numele coloanelor din metadata
    for (uint32_t i = 0; i < metadata->num_columns; i++) {
        stmt->insertStmt.columns[i] = strdup(metadata->column_names[i]);
    }
    
    // Actualizează statement-ul
    stmt->insertStmt.values = complete_values;
    stmt->insertStmt.num_values = metadata->num_columns;
    stmt->insertStmt.num_columns = metadata->num_columns;
}

void process_statement(Statement *stmt) {

    switch (stmt->type) {
        case STATEMENT_INSERT: {
            global_id = 1;
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

            replace_null_values(stmt, metadata);
            if(stmt->insertStmt.columns != NULL){
                preprocess_insert_statement(stmt, metadata);
            }
            if(root!=NULL){
                if(verify_constraints(stmt, metadata) == false) {
                    printf("        Error: Constraints not met\n");
                    break;
                }
            }

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
            strcat(dbfilepath, "/btree");
            strcat(dbfilepath, stmt->selectStmt.table); 
            strcat(dbfilepath, ".bin");
            // printf("%s\n\n", dbfilepath);

            char datafilepath[256] = {0};
            strcpy(datafilepath, DATA_FILENAME);
            strcat(datafilepath, "/data");
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
            strcat(dbfilepath, "/btree");
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
            strcat(filepath_db, "/btree");
            strcat(filepath_db, stmt->dropStmt.table);
            strcat(filepath_db, ".bin");
            
            strcpy(filepath_df, DB_FILENAME);
            strcat(filepath_df, "/data");
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
            char dbfilepath[256] = {0};
            strcpy(dbfilepath, "../databases");
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

            char dbfilepath[256] = {0};
            strcpy(dbfilepath, "../databases");
            strcat(dbfilepath, "/");
            strcat(dbfilepath, stmt->dropDbStmt.database);

            if(strcmp(DB_FILENAME, dbfilepath) == 0) {
                strcpy(DB_FILENAME, "../databases");
            }

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
    if (!stmt) return;
    
    switch (stmt->type) {
        case STATEMENT_INSERT:
            if (stmt->insertStmt.columns) {
                for (int i = 0; i < stmt->insertStmt.num_columns; i++) {
                    free(stmt->insertStmt.columns[i]);
                }
                free(stmt->insertStmt.columns);
            }

            if (stmt->insertStmt.values) {
                for (int i = 0; i < stmt->insertStmt.num_values; i++) {
                    free(stmt->insertStmt.values[i].value);
                    free(stmt->insertStmt.values[i].valueType);
                }
                free(stmt->insertStmt.values);
            }

            free(stmt->insertStmt.table);
            break;
            
        case STATEMENT_SELECT:
            if (stmt->selectStmt.columns) {
                for (int i = 0; i < stmt->selectStmt.num_columns; i++) {
                    free(stmt->selectStmt.columns[i]);
                }
                free(stmt->selectStmt.columns);
            }
            free(stmt->selectStmt.table);
            if (stmt->selectStmt.condition) {
                free(stmt->selectStmt.condition);
            }
            break;
            
        case STATEMENT_CREATE:
            if (stmt->createStmt.columns) {
                for (int i = 0; i < stmt->createStmt.num_columns; i++) {
                    free(stmt->createStmt.columns[i].column_name);
                    free(stmt->createStmt.columns[i].type);
                }
                free(stmt->createStmt.columns);
            }
            free(stmt->createStmt.table);
            break;
            
        case STATEMENT_DROP:
            free(stmt->dropStmt.table);
            break;
            
        case STATEMENT_CREATE_DB:
            free(stmt->createDbStmt.database);
            break;
            
        case STATEMENT_DROP_DB:
            free(stmt->dropDbStmt.database);
            break;
            
        case STATEMENT_USE_DB:
            free(stmt->useDbStmt.database);
            break;
            
        case STATEMENT_SHOW_DB:
        case STATEMENT_SHOW_TB:
            // Nu avem nimic de eliberat pentru aceste tipuri
            break;
            
        case STATEMENT_DESC_TB:
            free(stmt->descTbStmt.table);
            break;
            
        default:
            // Pentru orice alt tip necunoscut, nu facem nimic
            break;
    }
    root = NULL;
    visited_count = 0;
    serialized_count = 0;
    global_id = 1;
}


bool verify_constraints(Statement *stmt, MetadataPage *metadata) {

    for(int i = 0; i < metadata->num_columns; i++) {

        // printf("Column constraint: %d\n", metadata->column_constraints[i]);


        if (metadata->column_constraints[i] == CONSTRAINT_NOT_NULL) {
            // Check if the value is NULL
            if (stmt->insertStmt.values[i].value == NULL) {
                printf("Error: Column %s cannot be NULL\n", metadata->column_names[i]);
                return false;
            }
        } else if (metadata->column_constraints[i] == CONSTRAINT_UNIQUE) {
            bool is_unique = constraint_unique(root, stmt, metadata, i);
            if (!is_unique) {
                printf("Error: Column \'%s\' must be unique\n", metadata->column_names[i]);
                return false;
            }
        } else if (metadata->column_constraints[i] == CONSTRAINT_PRIMARY_KEY) {
            if (stmt->insertStmt.values[i].value == NULL) {
                printf("Error: Column %s cannot be NULL\n", metadata->column_names[i]);
                return false;
            }
            bool is_unique = constraint_unique(root, stmt, metadata, i);
            if (!is_unique) {
                printf("Error: Column \'%s\' must be unique\n", metadata->column_names[i]);
                return false;
            }
        }
    }
    // Implement constraint verification logic here
    // For example, check if the values meet the constraints defined in the metadata
    // This is a placeholder function and should be implemented based on your requirements

    return true;
}