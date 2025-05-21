#include "../include/core/database_ops.h"

void database_load() {
    root = load_btree_from_disk(db, metadata);
    if (!root) {
        printf("Failed to load B-Tree.\n");
    }

    load_datafile(df);
}




bool is_database_empty() {
    typedef struct {
        uint64_t magic;
        uint64_t write_ptr_offset;
    } DataFileHeader;

    DataFileHeader header;
    memcpy(&header, df->start_ptr, sizeof(header));

    return header.magic != MAGIC_NUMBER;
}



void database_init(char table_name[]) {
    db=NULL;
    df=NULL;
    metadata=NULL;
    free_page_queue=NULL;
    char dbfilepath[256] = {0};
    strcpy(dbfilepath, DB_FILENAME);
    strcat(dbfilepath, "/btree");
    strcat(dbfilepath, table_name); 
    strcat(dbfilepath, ".bin");

    char datafilepath[256] = {0};
    strcpy(datafilepath, DB_FILENAME);
    strcat(datafilepath, "/data");
    strcat(datafilepath, table_name);
    strcat(datafilepath, ".bin");

    if(!((db = malloc(sizeof(DBFile))))){
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!((df = malloc(sizeof(DataFile))))){
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!((metadata = malloc(sizeof(MetadataPage))))){
        perror("Metadata allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!(~access(dbfilepath, F_OK)))
        create_database_file(dbfilepath);
    
    

    if(!(~access(datafilepath, F_OK)))
        create_database_file(datafilepath);
    
    open_database_file(&db->fd,dbfilepath);
    open_database_file(&df->fd,datafilepath);
    
    get_db_file_size(db);
    get_df_file_size(df);

    if(!db->size)
        init_create_db_memory_block(db);

    if(!df->size)
        init_create_df_memory_block(df);


    memory_map_db_file(db);
    memory_map_df_file(df);

    set_file_dirty_db(db, false);
    set_file_dirty_df(df, false);
    set_new_file_free_blocks(db);


    free_page_queue = create_queue();

    for(int i=1;i<=20768;i+=1){
        push(free_page_queue,i);
    }
    if (!is_database_empty()) {
        database_load();
    }
    // else {
    //     printf("Database is empty!\n");
    // }
}