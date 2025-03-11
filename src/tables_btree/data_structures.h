#include "libraries.h"
#include "config.h"

typedef struct TableNode {
    u_int8_t is_leaf;
    u_int16_t num_keys; //nr de chei curente stocate in nod
    u_int16_t root_page[TABLE_MAX_KEYS];//pointeri catre root-ul tabelelor
    char keys[TABLE_MAX_KEYS][TABLE_NAME_LENGTH]; // cheile
    u_int64_t link[TABLE_MAX_KEYS+1]; //pointer catre nodurile copil
    struct TableNode* plink[];
}TableNode;

typedef struct DBFile {
    int fd;         // File descriptor for the database
    ssize_t size;    // Size of the mapped file
    void *data;     // Pointer to the mapped memory region
    bool dirty;      // Flag to track if changes were made
    int free_blocks; // Number of free blocks
}DBFile;

struct TableNode* root;