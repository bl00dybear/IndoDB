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

void set_new_file_free_blocks(db_file* db){
    db->free_blocks = 0;
}

void create_memory_block(db_file* db){
    if(!(~ftruncate(db->fd, INITIAL_DB_SIZE))){
        perror("Error creating memory block");
        exit(EXIT_FAILURE);
    }
    db->size += 1;

}

void write_on_memory_block(db_file *db, void* new_data){
    int block_index;
    if(!db->free_blocks){
        create_memory_block(db);
        block_index = db->size;
    }
    else{
        //implement queue tp store free blocks
    }  

    void* page = (char*)db->data + (block_index * PAGE_SIZE);
    memcpy(page, new_data, PAGE_SIZE);
    db->dirty = 1;
}




















// Function to create a new node with a given value
// Parameters:
// - val: The value to insert
// - child: Pointer to the right child node
struct node *create_node(const int val, struct node *child) {
    struct node *newNode = (struct node *)malloc(sizeof(struct node));
    newNode->block_index[1] = val;
    newNode->count = 1;
    newNode->link[0] = root;
    newNode->link[1] = child;
    return newNode;
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

// Function to split a node when it exceeds the maximum limit
// Parameters:
// - val: The value to insert
// - pval: Pointer to store the value that will move up
// - pos: Position of insertion in the current node
// - node: Pointer to the node to be split
// - child: Pointer to the child node associated with val
// - newNode: Pointer to the newly created node
void split_node(const int val, int *pval, const int pos, struct node *node, struct node *child, struct node **newNode) {
    int median;

    // Determine the median index to split
    if (pos > NODE_MIN_KEYS)
        median = NODE_MIN_KEYS + 1;
    else
        median = NODE_MIN_KEYS;

    // Allocate memory for the new right node
    *newNode = (struct node *)malloc(sizeof(struct node));
    int j = median + 1;

    // Move values and child pointers from the original node to the new node
    while (j <= NODE_MAX_KEYS) {
        (*newNode)->block_index[j - median] = node->block_index[j];
        (*newNode)->link[j - median] = node->link[j];
        j += 1;
    }

    // Update counts
    node->count = median;
    (*newNode)->count = NODE_MAX_KEYS - median;

    // Insert the new value into the appropriate node (left or right)
    if (pos <= NODE_MIN_KEYS) {
        insert_node(val, pos, node, child);
    } else {
        insert_node(val, pos - median, *newNode, child);
    }

    // Move median value up to parent
    *pval = node->block_index[node->count];

    // Adjust child pointers
    (*newNode)->link[0] = node->link[node->count];
    node->count--;
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

// Function to search for a value in the B-tree
// Parameters:
// - val: The value to search for
// - pos: Pointer to store the position of the value if found
// - myNode: Pointer to the current node being checked
void search(const int val, int *pos, struct node *myNode) {
    if (!myNode) {
        printf("%d not found in the tree\n", val);
        return;
    }

    // Find the correct position
    if (val < myNode->block_index[1]) {
        *pos = 0;
    } else {
        for (*pos = myNode->count; (val < myNode->block_index[*pos] && *pos > 1); (*pos)--)
            ;

        // If found, print message and return
        if (val == myNode->block_index[*pos]) {
            printf("%d is found\n", val);
            return;
        }
    }
    // Recursively search in the correct subtree
    search(val, pos, myNode->link[*pos]);
}


// Function to perform an in-order traversal of the B-tree
// Parameter:
// - myNode: Pointer to the node being traversed
void traversal(const struct node *myNode) {
    if (myNode) {
        int i;

        // Visit left subtree, print value, then right subtree
        for (i = 0; i < myNode->count; i++) {
            traversal(myNode->link[i]);
            printf("%d ", myNode->block_index[i + 1]);
        }

        traversal(myNode->link[i]);
    }
}


// Function to find the in-order predecessor (largest value in left subtree)
// Parameter:
// - node: Pointer to the subtree where predecessor is searched
// Returns: The predecessor value
int find_predecessor(const struct node *node) {
    while (node->link[node->count] != NULL)
        node = node->link[node->count];  // Move to the rightmost child
    return node->block_index[node->count];
}

// Function to find the in-order successor (smallest value in right subtree)
// Parameter:
// - node: Pointer to the subtree where successor is searched
// Returns: The successor value
int find_successor(const struct node *node) {
    while (node->link[0] != NULL)
        node = node->link[0];  // Move to the leftmost child
    return node->block_index[1];
}

// Function to shift values and child pointers left after deletion
// This function is used when a value is removed from a node, ensuring
// that all remaining values are shifted left to maintain order.
// Parameters:
// - node: Pointer to the node where shifting occurs
// - pos: The position from where values and child pointers need to be shifted
void shift_left(struct node *node, const int pos) {
    for (int i = pos; i < node->count; i++) {
        node->block_index[i] = node->block_index[i + 1];
        node->link[i] = node->link[i + 1];
    }
    node->link[node->count] = node->link[node->count + 1];
    node->count--;
}

// Function to delete a value from the B-tree
// Parameters:
// - node: Pointer to the current node being checked
// - val: The value to delete
void delete_value(struct node *node, const int val) {
    int pos;

    // Find position of value in current node
    if (val < node->block_index[1]) {
        pos = 0;
    } else {
        for (pos = node->count; (val < node->block_index[pos] && pos > 1); pos--);
    }

    // If value is found in this node
    if (val == node->block_index[pos]) {
        if (node->link[pos - 1] == NULL) {
            // Case 1: Leaf node - Simply remove the value
            shift_left(node, pos);
        } else {
            // Case 2: Internal node - Replace with predecessor or successor
            if (node->link[pos - 1] != NULL) {
                const int pred = find_predecessor(node->link[pos - 1]);
                node->block_index[pos] = pred;
                delete_value(node->link[pos - 1], pred);
            } else {
                const int succ = find_successor(node->link[pos]);
                node->block_index[pos] = succ;
                delete_value(node->link[pos], succ);
            }
        }
    } else {
        // If value is not found in this node, search in the correct subtree
        if (node->link[pos] == NULL) {
            printf("Value %d not found in the tree\n", val);
            return;
        }
        delete_value(node->link[pos], val);
    }

    // If node becomes underfull, handle merging or borrowing (not implemented fully here)
}

// Function to delete a value from the B-tree
// Parameter:
// - val: The value to delete
void delete(const int val) {
    if (root == NULL) {
        printf("Tree is empty\n");
        return;
    }

    delete_value(root, val);

    // If root has no values left, adjust the root (if tree shrinks)
    if (root->count == 0) {
        struct node *temp = root;
        root = root->link[0];
        free(temp);
    }
}

// Function to insert a value into the B-tree
// Parameter:
// - val: The value to insert
void insert(const int val){
    int i;
    struct node *child;

    const int flag = set_value(val, &i, root, &child);

    if(flag)
        root = create_node(i, child);
}



void cli_interactions(db_file* db){
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

    switch(choice){
        case 1:
            printf("Enter a value to insert: ");
            int val;
            scanf("%d", &val);
            insert(val);
            char new_data[PAGE_SIZE];
            memset(new_data, (char)(val - '0'), PAGE_SIZE);
            printf("%s",new_data);
            write_on_memory_block(db, new_data);
            break;
        case 2:
            printf("Enter a value to delete: ");
            int del;
            scanf("%d", &del);
            delete(del);
            break;
        case 3:
            printf("Enter a value to search: ");
            int search_val;
            scanf("%d", &search_val);
            int pos;
            search(search_val, &pos, root);
            break;
        case 4:
            traversal(root);
            printf("\n");
            break;
        case 5:
            // Commit changes
            break;
        case 6:
            exit = true;
            break;
        default:
            printf("Invalid choice\n");
    }
    }


    


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
    set_new_file_free_blocks(db);

    printf("%d %ld\n",db->fd,db->size);

    cli_interactions(db);

    free(db);
    exit(EXIT_SUCCESS);
}