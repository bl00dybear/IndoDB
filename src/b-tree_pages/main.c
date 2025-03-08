#include "data_structures.h"

void create_database_file(){
    if(!(~creat(DB_FILENAME, 0644))){
        perror("Error creating database file");
        exit(EXIT_FAILURE);
    }
}

void open_database_file(int* db_filedescriptor){
    if(!(~((*db_filedescriptor) = open(DB_FILENAME, O_RDWR | O_CREAT, 0644)))){
        perror("Error opening database file");
        exit(EXIT_FAILURE);
    }
}

void get_file_size(db_file* db){
    struct stat st;
    if(!(~fstat(db->fd, &st))){
        perror("Error getting file size");
        exit(EXIT_FAILURE);
    }
    db->size = st.st_size;
}

void memory_map_file(db_file* db){
    if((db->data = mmap(NULL, db->size, PROT_READ | PROT_WRITE, MAP_SHARED, db->fd, 0)) == MAP_FAILED){
        perror("Error mapping database file");
        exit(EXIT_FAILURE);
    }
}

void set_file_dirty(db_file* db, bool dirty){
    db->dirty = dirty;
}

void create_memory_block(db_file* db){
    if(!(~ftruncate(db->fd, INITIAL_DB_SIZE))){
        perror("Error creating memory block");
        exit(EXIT_FAILURE);
    }
    db->size += 1;

}


// Function to insert a value into a node at the given position
// Parameters:
// - val: The value to insert
// - pos: The position where value should be inserted
// - node: Pointer to the node in which value will be inserted
// - child: Pointer to the child node associated with the value
void insert_node(const int val, const int pos, struct node *node, struct node *child) {
    int j = node->count;

    // Shift values and child pointers to make space for the new value
    while (j > pos) {
        node->block_index[j + 1] = node->block_index[j];
        node->link[j + 1] = node->link[j];
        j -= 1;
    }

    node->block_index[j + 1] = val;  // Insert value at correct position
    node->link[j + 1] = child;  // Adjust child link
    node->count += 1;  // Increment count of values
}


// Function to determine where to insert a value in the B-tree
// Parameters:
// - val: The value to insert
// - pval: Pointer to store the promoted value
// - node: Pointer to the current node being checked
// - child: Pointer to store the new child node if split occurs
int set_value(const int val, int *pval, struct node *node,struct node **child){
    int pos;

    //node receive initially root pointer
    // if tree is empty, create new root
    if(!node){
        *pval = val, *child = NULL;
        return 1;
    }

    // finding correct position for the value in the current node
    if (val < node->block_index[1]) {
        pos = 0;
    } else {
        for (pos = node->count; (val < node->block_index[pos] && pos > 1); pos--)
            ;

        // Prevent duplicate values
        if (val == node->block_index[pos]) {
            printf("Duplicates are not permitted\n");
            return 0;
        }
    }

    // Recursively insert into the correct subtree
    if (set_value(val, pval, node->link[pos], child)) {
        // If node has space, insert normally
        if (node->count < NODE_MAX_KEYS) {
            insert_node(*pval, pos, node, *child);
        } else {
            // Otherwise, split the node
            split_node(*pval, pval, pos, node, *child, child);
            return 1;
        }
    }
    return 0;

}


// Function to insert a value into the B-tree
// Parameter:
// - val: The value to insert
void insert_node(const int val){
    int i;
    struct node *child;

    const int flag = set_value(val, &i, root, &child);

    if(flag)
        root = create_node(i, child);
}



int main(){
    // Step 1: Verify if the file exists in directory
    db_file* db;
    if(!(db = malloc(sizeof(db_file)))){
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    if(!(~access(DB_FILENAME, F_OK)))
        create_database_file(&db->fd);

    open_database_file(&db->fd);

    if(!db->size)
        create_memory_block(db);

    memory_map_file(db);

    set_file_dirty(db, false);

    printf("%d %ld\n",db->fd,db->size);

    free(db);
    exit(EXIT_SUCCESS);
}