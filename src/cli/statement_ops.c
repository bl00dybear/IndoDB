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
void diagnose_display_table(const char* checkpoint, char** columns, int num_columns, 
                           MetadataPage *metadata, Statement *stmt) {
    printf("\n============ DIAGNOSTIC %s ============\n", checkpoint);
    
    // 1. Verificare parametri de intrare
    printf("\n[1] PARAMETRI DE INTRARE:\n");
    printf("  • columns: %p (NULL? %s)\n", (void*)columns, columns ? "Nu" : "Da");
    printf("  • num_columns: %d\n", num_columns);
    printf("  • metadata: %p (NULL? %s)\n", (void*)metadata, metadata ? "Nu" : "Da");
    printf("  • stmt: %p (NULL? %s)\n", (void*)stmt, stmt ? "Nu" : "Da");
    
    // 2. Verificare conținut columns dacă există
    if (columns && num_columns > 0) {
        printf("\n[2] CONȚINUT COLUMNS:\n");
        for (int i = 0; i < num_columns && i < 10; i++) {
            if (columns[i]) {
                printf("  • columns[%d]: %p → %s\n", i, (void*)columns[i], columns[i]);
            } else {
                printf("  • columns[%d]: NULL\n", i);
            }
        }
        if (num_columns > 10) printf("  • [...și încă %d coloane]\n", num_columns - 10);
    }
    
    // 3. Verificare metadata detaliată
    if (metadata) {
        printf("\n[3] METADATA DETALIATĂ:\n");
        printf("  • magic: 0x%lx\n", metadata->magic);
        printf("  • table_name: %s\n", metadata->table_name);
        printf("  • num_columns: %u\n", metadata->num_columns);
        printf("  • root_page_num: %lu\n", metadata->root_page_num);
        printf("  • last_table_id: %lu\n", metadata->last_table_id);
        
        // Verificare coloane cu mai multe detalii
        if (metadata->num_columns > 0 && metadata->num_columns <= MAX_COLUMNS) {
            printf("\n[3.1] COLOANE ÎN METADATA:\n");
            for (uint32_t i = 0; i < metadata->num_columns && i < 10; i++) {
                printf("  • column[%u]: name=%s, type=%d (", i, metadata->column_names[i], metadata->column_types[i]);
                
                // Afișează tipul de date ca text
                switch (metadata->column_types[i]) {
                    case TYPE_INT: printf("INT"); break;
                    case TYPE_FLOAT: printf("FLOAT"); break;
                    case TYPE_VARCHAR: printf("VARCHAR"); break;
                    case TYPE_TIMESTAMP: printf("TIMESTAMP"); break;
                    default: printf("UNKNOWN"); break;
                }
                
                printf("), size=%u, constraint=%d (", metadata->column_sizes[i], metadata->column_constraints[i]);
                
                // Afișează constrângerea ca text
                switch (metadata->column_constraints[i]) {
                    case CONSTRAINT_NONE: printf("NONE"); break;
                    case CONSTRAINT_NOT_NULL: printf("NOT NULL"); break;
                    case CONSTRAINT_UNIQUE: printf("UNIQUE"); break;
                    case CONSTRAINT_PRIMARY_KEY: printf("PRIMARY KEY"); break;
                    case CONSTRAINT_FOREIGN_KEY: printf("FOREIGN KEY"); break;
                    default: printf("UNKNOWN"); break;
                }
                
                printf(")\n");
            }
            if (metadata->num_columns > 10) {
                printf("  • [...și încă %d coloane]\n", metadata->num_columns - 10);
            }
        } else if (metadata->num_columns > MAX_COLUMNS) {
            printf("  • EROARE: num_columns (%u) depășește MAX_COLUMNS (%d)!\n", 
                  metadata->num_columns, MAX_COLUMNS);
        }
        
        // Verificare bitmap pentru pagini libere (primii 8 bytes)
        printf("\n[3.2] FREE PAGE BITMAP (primii 8 bytes):\n  • ");
        for (int i = 0; i < 8 && i < sizeof(metadata->free_page_bitmap); i++) {
            printf("%02x ", metadata->free_page_bitmap[i]);
        }
        printf("\n");
    }
    
    // 4. Verificare stare stmt detaliată
    if (stmt) {
        printf("\n[4] STATEMENT DETALIAT (tip: %d):\n", stmt->type);
        
        switch (stmt->type) {
            case STATEMENT_SELECT:
                printf("  • Tip: SELECT\n");
                printf("  • table: %s\n", stmt->selectStmt.table ? stmt->selectStmt.table : "NULL");
                printf("  • num_columns: %d\n", stmt->selectStmt.num_columns);
                
                if (stmt->selectStmt.columns) {
                    printf("\n[4.1] COLOANE ÎN SELECT:\n");
                    for (int i = 0; i < stmt->selectStmt.num_columns && i < 10; i++) {
                        if (stmt->selectStmt.columns[i]) {
                            printf("  • column[%d]: %s\n", i, stmt->selectStmt.columns[i]);
                        } else {
                            printf("  • column[%d]: NULL\n", i);
                        }
                    }
                    if (stmt->selectStmt.num_columns > 10) {
                        printf("  • [...și încă %d coloane]\n", stmt->selectStmt.num_columns - 10);
                    }
                } else {
                    printf("  • columns: NULL\n");
                }
                
                printf("\n[4.2] CONDIȚIE ȘI COLOANE ÎN CONDIȚIE:\n");

                // Verifică adresa condiției fără a o dereferenția

                // Folosește accesare sigură
                if (stmt->selectStmt.condition == NULL) {
                    printf("Condition este mereu NULL pentru ca exista functia collect_columns_from_condition\n");
                } else {
                    printf("  • condition: n-are cum sa ajunge aici\n");
                }
                
                if (stmt->selectStmt.cond_column) {
                    printf("  • num_cond_columns: %d\n", stmt->selectStmt.num_cond_columns);
                    for (int i = 0; i < stmt->selectStmt.num_cond_columns && i < 10; i++) {
                        if (stmt->selectStmt.cond_column[i]) {
                            printf("  • cond_column[%d]: %s\n", i, stmt->selectStmt.cond_column[i]);
                        } else {
                            printf("  • cond_column[%d]: NULL\n", i);
                        }
                    }
                    if (stmt->selectStmt.num_cond_columns > 10) {
                        printf("  • [...și încă %d coloane în condiție]\n", 
                              stmt->selectStmt.num_cond_columns - 10);
                    }
                } else {
                    printf("  • cond_column: NULL\n");
                }
                break;
                
            case STATEMENT_INSERT:
                printf("  • Tip: INSERT\n");
                printf("  • table: %s\n", stmt->insertStmt.table ? stmt->insertStmt.table : "NULL");
                printf("  • num_columns: %d\n", stmt->insertStmt.num_columns);
                printf("  • num_values: %d\n", stmt->insertStmt.num_values);
                break;
                
            case STATEMENT_CREATE:
                printf("  • Tip: CREATE TABLE\n");
                printf("  • table: %s\n", stmt->createStmt.table ? stmt->createStmt.table : "NULL");
                printf("  • num_columns: %d\n", stmt->createStmt.num_columns);
                break;
                
            case STATEMENT_DROP:
                printf("  • Tip: DROP TABLE\n");
                printf("  • table: %s\n", stmt->dropStmt.table ? stmt->dropStmt.table : "NULL");
                break;
                
            case STATEMENT_CREATE_DB:
                printf("  • Tip: CREATE DATABASE\n");
                printf("  • database: %s\n", stmt->createDbStmt.database ? stmt->createDbStmt.database : "NULL");
                break;
                
            case STATEMENT_DROP_DB:
                printf("  • Tip: DROP DATABASE\n");
                printf("  • database: %s\n", stmt->dropDbStmt.database ? stmt->dropDbStmt.database : "NULL");
                break;
                
            case STATEMENT_USE_DB:
                printf("  • Tip: USE DATABASE\n");
                printf("  • database: %s\n", stmt->useDbStmt.database ? stmt->useDbStmt.database : "NULL");
                break;
                
            case STATEMENT_SHOW_DB:
                printf("  • Tip: SHOW DATABASES\n");
                break;
                
            case STATEMENT_SHOW_TB:
                printf("  • Tip: SHOW TABLES\n");
                break;
                
            case STATEMENT_DESC_TB:
                printf("  • Tip: DESCRIBE TABLE\n");
                printf("  • table: %s\n", stmt->descTbStmt.table ? stmt->descTbStmt.table : "NULL");
                break;
                
            default:
                printf("  • Tip: NECUNOSCUT (%d)\n", stmt->type);
                break;
        }
    }
    
    // 5. Verificare stare globală
    printf("\n[5] STARE GLOBALĂ:\n");
    printf("  • root: %p (NULL? %s)\n", (void*)root, root ? "Nu" : "Da");
    printf("  • df: %p (NULL? %s)\n", (void*)df, df ? "Nu" : "Da");
    printf("  • global_id: %lu\n", global_id);
    printf("  • visited_count: %d\n", visited_count);
    printf("  • serialized_count: %d\n", serialized_count);
    printf("  • free_page_queue: %p (NULL? %s)\n", 
          (void*)free_page_queue, free_page_queue ? "Nu" : "Da");
    
    // Verifică starea queue-ului de pagini libere
    if (free_page_queue) {
        printf("  • free_page_queue->front: %p\n", (void*)free_page_queue->front);
        printf("  • free_page_queue->rear: %p\n", (void*)free_page_queue->rear);
        
        // Verifică dacă există elemente în queue
        if (free_page_queue->front) {
            int count = 0;
            QueueNode* current = free_page_queue->front;
            printf("  • Primele pagini libere: ");
            
            while (current && count < 5) {
                printf("%d ", current->data);
                current = current->next;
                count++;
            }
            
            if (current) {
                printf("...\n");
            } else {
                printf("\n");
            }
        } else {
            printf("  • Queue gol (front == NULL)\n");
        }
    }
    
    // 6. Verificare stare Root B-tree
    if (root) {
        printf("\n[6] ROOT B-TREE DETALIAT:\n");
        printf("  • Adresă: %p\n", (void*)root);
        printf("  • is_leaf: %u\n", root->is_leaf);
        printf("  • num_keys: %u (valid? %s)\n", root->num_keys, 
              (root->num_keys <= ROW_MAX_KEYS) ? "Da" : "Nu");
        printf("  • page_num: %lu\n", root->page_num);
        
        // Verificare posibile probleme
        if (root->num_keys > ROW_MAX_KEYS) {
            printf("  • EROARE CRITICĂ: num_keys (%u) > ROW_MAX_KEYS (%d)!\n", 
                  root->num_keys, ROW_MAX_KEYS);
        }
        
        // Verificare keys și raw_data
        if (root->num_keys > 0 && root->num_keys <= ROW_MAX_KEYS) {
            printf("\n[6.1] CHEI ȘI DATE:\n");
            for (int i = 1; i <= root->num_keys && i <= 5; i++) {
                printf("  • key[%d]: %lu, raw_data[%d]: %p\n", 
                      i, root->keys[i], i, root->raw_data[i]);
                
                // Verifică validitatea pointer-ului raw_data
                if (root->raw_data[i] && df && df->start_ptr) {
                    uint64_t offset = (uint64_t)root->raw_data[i];
                    void* row_ptr = (void*)(df->start_ptr + offset);
                    
                    // Verifică că offset-ul este în interiorul fișierului
                    if (offset < df->size) {
                        uint64_t row_size = 0;
                        memcpy(&row_size, row_ptr, sizeof(uint64_t));
                        printf("    - row_size: %lu bytes\n", row_size);
                        
                        // Verifică dacă dimensiunea rândului este rezonabilă
                        if (row_size < 9 || row_size > 1024*1024) {
                            printf("    - AVERTISMENT: Dimensiune rând suspectă!\n");
                        }
                    } else {
                        printf("    - EROARE: Offset în afara limitelor fișierului!\n");
                    }
                }
            }
            
            if (root->num_keys > 5) {
                printf("  • [...și încă %u chei]\n", root->num_keys - 5);
            }
        }
        
        // Verificare pointeri copii
        printf("\n[6.2] POINTERI COPII:\n");
        int valid_children = 0;
        for (int i = 0; i <= root->num_keys && i <= 5; i++) {
            printf("  • plink[%d]: %p (NULL? %s)\n", i, (void*)root->plink[i], 
                  root->plink[i] ? "Nu" : "Da");
            
            // Verifică pointeri suspecți
            if (root->plink[i]) {
                valid_children++;
                if ((uintptr_t)root->plink[i] < 1000) {
                    printf("    - AVERTISMENT: Valoare suspectă pentru pointer!\n");
                }
                
                // Verifică link
                printf("    - link[%d]: %lu\n", i, root->link[i]);
                
                // Verifică nodul copil dacă există
                if (root->plink[i]->num_keys > ROW_MAX_KEYS) {
                    printf("    - EROARE: Nodul copil are num_keys invalid (%u)!\n", 
                          root->plink[i]->num_keys);
                }
            }
        }
        
        if (root->num_keys + 1 > 5) {
            printf("  • [...și încă %u pointeri]\n", root->num_keys + 1 - 5);
        }
        
        printf("  • Total copii valizi: %d\n", valid_children);
    }
    
    // 7. Verificare stare DataFile
    if (df) {
        printf("\n[7] DATAFILE STATUS DETALIAT:\n");
        printf("  • Adresă: %p\n", (void*)df);
        printf("  • fd: %d\n", df->fd);
        printf("  • start_ptr: %p\n", df->start_ptr);
        printf("  • size: %zu bytes (%.2f MB)\n", df->size, (float)df->size / (1024 * 1024));
        printf("  • write_ptr: %lu (%.2f%% din fișier)\n", 
              df->write_ptr, (float)df->write_ptr * 100 / (df->size ? df->size : 1));
        printf("  • dirty: %s\n", df->dirty ? "Da (modificări nesalvate)" : "Nu (fișier sincronizat)");
        
        // Verificare header fișier (primii bytes)
        if (df->start_ptr) {
            printf("\n[7.1] HEADER FIȘIER (primii 16 bytes):\n  • ");
            unsigned char* header = (unsigned char*)df->start_ptr;
            for (int i = 0; i < 16 && i < df->size; i++) {
                printf("%02x ", header[i]);
                if (i == 7) printf(" | ");
            }
            printf("\n");
        }
    }
    
    // 8. Verificare stare memorie și structuri indexare
    printf("\n[8] STARE MEMORIE ȘI STRUCTURI INDEXARE:\n");
    
    if (visited_count >= 0 && visited_count < MAX_VISITED_NODES) {
        printf("  • visited_nodes: %d noduri înregistrate\n", visited_count);
        
        // Verifică primele câteva noduri vizitate
        for (int i = 0; i < visited_count && i < 3; i++) {
            printf("    - visited_nodes[%d]: page_num=%lu, node=%p\n", 
                  i, visited_nodes[i].page_num, (void*)visited_nodes[i].node);
        }
        
        if (visited_count > 3) {
            printf("    - [...și încă %d noduri vizitate]\n", visited_count - 3);
        }
    } else if (visited_count >= MAX_VISITED_NODES) {
        printf("  • AVERTISMENT: visited_count (%d) >= MAX_VISITED_NODES (%d)!\n", 
              visited_count, MAX_VISITED_NODES);
    }
    
    if (serialized_count >= 0 && serialized_count < MAX_VISITED_NODES) {
        printf("  • serialized_pages: %d pagini serializate\n", serialized_count);
        
        // Verifică primele câteva pagini serializate
        for (int i = 0; i < serialized_count && i < 3; i++) {
            printf("    - serialized_pages[%d]: %lu\n", i, serialized_pages[i]);
        }
        
        if (serialized_count > 3) {
            printf("    - [...și încă %d pagini serializate]\n", serialized_count - 3);
        }
    } else if (serialized_count >= MAX_VISITED_NODES) {
        printf("  • AVERTISMENT: serialized_count (%d) >= MAX_VISITED_NODES (%d)!\n", 
              serialized_count, MAX_VISITED_NODES);
    }
    
    // Verifică heap și stack
    void* stack_var = &checkpoint;
    void* heap_var = malloc(1);
    printf("  • Adresă stack: %p\n", (void*)stack_var);
    printf("  • Adresă heap: %p\n", heap_var);
    free(heap_var);
    
    printf("\n============ END DIAGNOSTIC ============\n");
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

                    // Înainte de display_table
                    // diagnose_display_table("ÎNAINTE DE AFIȘARE", column_pointers, metadata->num_columns, metadata, stmt);

                    // Apelul funcției originale
                    display_table(column_pointers, metadata->num_columns, metadata, stmt);

                    // Imediat după display_table
                    // diagnose_display_table("DUPĂ AFIȘARE", column_pointers, metadata->num_columns, metadata, stmt);

                    // printf("se intoarce aici\n");
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
            if (stmt->selectStmt.columns != NULL) {
                for (int i = 0; i < stmt->selectStmt.num_columns && stmt->selectStmt.columns[i] != NULL; i++) {
                    free(stmt->selectStmt.columns[i]);
                    stmt->selectStmt.columns[i] = NULL; // Prevenire double-free
                }
                free(stmt->selectStmt.columns);
                stmt->selectStmt.columns = NULL;
            }
            
            if (stmt->selectStmt.table != NULL) {
                free(stmt->selectStmt.table);
                stmt->selectStmt.table = NULL;
            }
            
            if (stmt->selectStmt.condition != NULL) {
                cJSON_Delete(stmt->selectStmt.condition);
                stmt->selectStmt.condition = NULL;
            }
            
            if (stmt->selectStmt.cond_column != NULL) {
                // Verificare pentru număr valid de coloane condiționale
                if (stmt->selectStmt.num_cond_columns > 0) {
                    for (int i = 0; i < stmt->selectStmt.num_cond_columns && 
                        stmt->selectStmt.cond_column[i] != NULL; i++) {
                        free(stmt->selectStmt.cond_column[i]);
                        stmt->selectStmt.cond_column[i] = NULL; // Prevenire double-free
                    }
                }
                free(stmt->selectStmt.cond_column);
                stmt->selectStmt.cond_column = NULL;
            }
            
            stmt->selectStmt.num_columns = 0;
            stmt->selectStmt.num_cond_columns = 0;
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