//#include "storage_mmap.c"
#include "pages/inital_page_handling.c"

int main() {
    DBFile* db = db_open(DB_FILENAME);

    // Start a transaction (save snapshot)
    db_start_transaction(db);

    // Modify a page in memory (write 'X' everywhere)
    int page_num = 0;
    char new_data[PAGE_SIZE];
    memset(new_data, 'A', PAGE_SIZE);
    db_write_page(db, page_num, new_data);

    // User chooses whether to commit or rollback
    char choice;
    printf("Commit changes? (y/n): ");
    scanf(" %c", &choice);

    if (choice == 'y') {
        db_commit(db);
    } else {
        db_rollback(db);
    }

    db_close(db);
    return 0;
}
    