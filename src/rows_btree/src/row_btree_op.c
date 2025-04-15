#include "../headers/row_btree_op.h"
#include "../headers/config.h"
#include "../headers/queue.h"



// Global variables

// Function to create a new node with a given value
struct RowNode *create_node(const int val, struct RowNode *child) {
    struct RowNode *newNode = (struct RowNode *)malloc(sizeof(struct RowNode));
    if (!newNode) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }
    
    // Initialize the node
    memset(newNode, 0, sizeof(struct RowNode));
    newNode->keys[1] = val;
    newNode->num_keys = 1;
    for (int i = 0; i < ROW_MAX_KEYS + 1; i++) {
    newNode->link[i] = 0;  // Initialize all links to 0
    }
    newNode->link[0] = root ? root->page_num : 0;
    newNode->link[1] = child ? child->page_num : 0;
    // Set links
    newNode->plink[0] = root;
    newNode->plink[1] = child;
    
    // Allocate page number from free page queue
    newNode->page_num = front(free_page_queue);
    pop(free_page_queue);
    
    // Set link values (page numbers)
    if(root != NULL)
        newNode->link[0] = root->page_num;
    else
        newNode->link[0] = 0;
    
    if(child != NULL)
        newNode->link[1] = child->page_num;
    else
        newNode->link[1] = 0;
    
    // Set is_leaf flag - a node is a leaf if it has no children
    // In B-tree, a node is a leaf if all its plink pointers are NULL
    newNode->is_leaf = (root == NULL && child == NULL);
    
    return newNode;
}

// Function to insert a value into a node at the given position
void insert_node(const int val, const int pos, struct RowNode *node, struct RowNode *child) {
    int j = node->num_keys;

    // Shift values and child pointers to make space for the new value
    while (j > pos) {
        node->keys[j + 1] = node->keys[j];
        node->plink[j + 1] = node->plink[j];
        node->link[j + 1] = node->link[j];
        j -= 1;
    }

    // Insert value at correct position
    node->keys[j + 1] = val;
    node->plink[j + 1] = child;
    
    // Update link if child exists
    if(child != NULL) {
        node->link[j + 1] = child->page_num;
        // Since we're adding a child, this node is no longer a leaf
        node->is_leaf = 0;
    } else {
        node->link[j + 1] = 0;
    }
    
    node->num_keys += 1;
}

// Function to split a node when it exceeds the maximum limit
void split_node(const int val, int *pval, const int pos, struct RowNode *node, struct RowNode *child, struct RowNode **newNode) {
    int median;

    // Determine the median index to split
    if (pos > ROW_MIN_KEYS)
        median = ROW_MIN_KEYS + 1;
    else
        median = ROW_MIN_KEYS;

    // Allocate memory for the new right node
    *newNode = (struct RowNode *)malloc(sizeof(struct RowNode));
    if (!*newNode) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }
    
    // Initialize the new node
    memset(*newNode, 0, sizeof(struct RowNode));
    
    // The new node inherits the leaf status of the original node
    (*newNode)->is_leaf = node->is_leaf;
    
    // Get page number for new node
    (*newNode)->page_num = front(free_page_queue);
    pop(free_page_queue);
    
    // Move values and child pointers from the original node to the new node
    int j = median + 1;
    while (j <= ROW_MAX_KEYS) {
        (*newNode)->keys[j - median] = node->keys[j];
        (*newNode)->plink[j - median] = node->plink[j];
        (*newNode)->link[j - median] = node->link[j];
        j += 1;
    }

    // Update num_keys
    node->num_keys = median;
    (*newNode)->num_keys = ROW_MAX_KEYS - median;

    // Insert the new value into the appropriate node (left or right)
    if (pos <= ROW_MIN_KEYS) {
        insert_node(val, pos, node, child);
    } else {
        insert_node(val, pos - median, *newNode, child);
    }

    // Move median value up to parent
    *pval = node->keys[node->num_keys];

    // Adjust child pointers
    (*newNode)->plink[0] = node->plink[node->num_keys];
    if (node->plink[node->num_keys] != NULL) {
        (*newNode)->link[0] = node->link[node->num_keys];
    } else {
        (*newNode)->link[0] = 0;
    }
    
    node->num_keys--;
}

// Function to determine where to insert a value in the B-tree
int set_value(const int val, int *pval, struct RowNode *node, struct RowNode **child) {
    int pos;

    // If tree is empty or we've reached a leaf, create new node
    if (!node) {
        *pval = val;
        *child = NULL;
        return 1;
    }

    // Finding correct position for the value in the current node
    if (val < node->keys[1]) {
        pos = 0;
    } else {
        for (pos = node->num_keys; (val < node->keys[pos] && pos > 1); pos--);
        
        // Prevent duplicate values (optional)
        // if (val == node->keys[pos]) {
        //     printf("Duplicates are not permitted\n");
        //     return 0;
        // }
    }

    // If node is a leaf, insert directly
    if (node->is_leaf) {
        if (node->num_keys < ROW_MAX_KEYS) {
            insert_node(val, pos, node, NULL);
            return 0;
        } else {
            // Split leaf node
            split_node(val, pval, pos, node, NULL, child);
            return 1;
        }
    } else {
        // For internal nodes, recursively insert into the correct subtree
        if (set_value(val, pval, node->plink[pos], child)) {
            // If child was split, insert promoted value
            if (node->num_keys < ROW_MAX_KEYS) {
                insert_node(*pval, pos, node, *child);
                return 0;
            } else {
                // Split internal node
                split_node(*pval, pval, pos, node, *child, child);
                return 1;
            }
        }
    }
    return 0;
}

// Function to search for a value in the B-tree
void search(const int val, int *pos, struct RowNode *myNode) {
    if (!myNode) {
        printf("%d not found in the tree\n", val);
        return;
    }

    // Find the correct position
    if (val < myNode->keys[1]) {
        *pos = 0;
    } else {
        for (*pos = myNode->num_keys; (val < myNode->keys[*pos] && *pos > 1); (*pos)--);

        // If found, print message and return
        if (val == myNode->keys[*pos]) {
            printf("%d is found\n", val);
            return;
        }
    }
    
    // If this is a leaf and we didn't find the value, it's not in the tree
    if (myNode->is_leaf) {
        printf("%d not found in the tree\n", val);
        return;
    }
    
    // Recursively search in the correct subtree
    search(val, pos, myNode->plink[*pos]);
}

// Function to perform an in-order traversal of the B-tree
void traversal(const struct RowNode *myNode) {
    if (myNode == NULL) {
        printf("Node is NULL\n");
        return;
    }
    
    printf("Node at page %lu, keys: %d, is_leaf: %s\n", 
           myNode->page_num, myNode->num_keys, 
           myNode->is_leaf ? "Yes" : "No");
    
    int i;
    for (i = 0; i < myNode->num_keys; i++) {
        printf("  Key[%d] = %ld\n", i+1, myNode->keys[i+1]);
    }
    
    // For debugging plink pointers after deserialization
    for (i = 0; i <= myNode->num_keys; i++) {
        if (myNode->plink[i] != NULL) {
            printf("  Child[%d] points to page %lu\n", i, myNode->plink[i]->page_num);
        } else if (myNode->link[i] != 0) {
            printf("  Child[%d] should point to page %lu but plink is NULL\n", 
                   i, myNode->link[i]);
        }
    }
    
    // Visit children if they exist
    for (i = 0; i <= myNode->num_keys; i++) {
        if (myNode->plink[i] != NULL) {
            printf("Traversing to child %d (page %lu):\n", i, myNode->plink[i]->page_num);
            traversal(myNode->plink[i]);
        }
    }
}

// Serialization functions

// Array pentru a urmări nodurile vizitate


// Funcție pentru a găsi un nod deja vizitat
RowNode* find_visited_node(uint64_t page_num) {
    for (int i = 0; i < visited_count; i++) {
        if (visited_nodes[i].page_num == page_num) {
            return visited_nodes[i].node;
        }
    }
    return NULL;
}

// Funcție pentru a adăuga un nod în lista de noduri vizitate
void add_visited_node(uint64_t page_num, RowNode* node) {
    if (visited_count < MAX_NODES) {
        visited_nodes[visited_count].page_num = page_num;
        visited_nodes[visited_count].node = node;
        visited_count++;
    } else {
        printf("Warning: Visited nodes array is full, can't track more nodes\n");
    }
}

// Variabile pentru a urmări paginile serializate


// Funcție pentru a verifica dacă o pagină a fost deja serializată
bool is_page_serialized(uint64_t page_num) {
    for (int i = 0; i < serialized_count; i++) {
        if (serialized_pages[i] == page_num) {
            return true;
        }
    }
    return false;
}

// Funcție pentru a adăuga o pagină în lista de pagini serializate
void add_serialized_page(uint64_t page_num) {
    if (serialized_count < MAX_NODES) {
        serialized_pages[serialized_count++] = page_num;
    }
}

// Improved serialize_node function
void serialize_node(DBFile* db, RowNode* node) {
    if (node == NULL) return;
    
    // Check if the node has already been serialized
    if (is_page_serialized(node->page_num)) {
        return;
    }
    
    // Mark the node as serialized
    add_serialized_page(node->page_num);

    // Allocate buffer for serialization
    void* buffer = calloc(1, PAGE_SIZE);
    if (!buffer) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }
    
    // Convert pointer links to page numbers
    for (int i = 0; i <= node->num_keys; i++) {
        if (node->plink[i] != NULL) {
            node->link[i] = node->plink[i]->page_num;
            printf("%ld %ld\n",node->page_num, node->link[i]);
        } else {
            node->link[i] = 0; // Explicitly set to 0 if no child
        }
    }
    
    // Serialize node data
    size_t offset = 0;
    memcpy(buffer + offset, &node->is_leaf, sizeof(node->is_leaf));
    offset += sizeof(node->is_leaf);
    
    memcpy(buffer + offset, &node->num_keys, sizeof(node->num_keys));
    offset += sizeof(node->num_keys);
    
    memcpy(buffer + offset, &node->page_num, sizeof(node->page_num));
    offset += sizeof(node->page_num);
    
    memcpy(buffer + offset, node->keys, sizeof(node->keys));
    offset += sizeof(node->keys);
    
    memcpy(buffer + offset, node->raw_data, sizeof(node->raw_data));
    offset += sizeof(node->raw_data);
    
    memcpy(buffer + offset, node->link, sizeof(node->link));

    // Write to disk
    write_on_memory_block(db, buffer, node->page_num);
    free(buffer);
    
    // Serialize child nodes
    for (int i = 0; i <= node->num_keys; i++) {
        if (node->plink[i] != NULL) {
            serialize_node(db, node->plink[i]);
        }
    }
}

// Funcție modificată pentru serializarea întregului arbore B
void serialize_btree(DBFile* db, RowNode* root) {
    if (!root) {
        printf("Error: Cannot serialize NULL root\n");
        return;
    }
    
    // Resetează array-ul de pagini serializate
    serialized_count = 0;

    // Salvează numărul paginii rădăcinii în metadata (pagina 0)
    uint64_t root_page_num = root->page_num;
    memcpy(db->data, &root_page_num, sizeof(root_page_num));
    printf("Serializing root page number %lu to metadata\n", root_page_num);

    // Serializează restul arborelui
    serialize_node(db, root);
    
    printf("Serialization complete. Total nodes serialized: %d\n", serialized_count);
}

// Funcție de deserializare a unui nod, modificată pentru a utiliza lista de noduri vizitate
RowNode* deserialize_node(DBFile* db, uint64_t page_num) {
    // Validate page number
    if (page_num == 0 || page_num * PAGE_SIZE >= db->size) {
        return NULL;
    }
    
    // Check if node is already loaded
    RowNode* existing_node = find_visited_node(page_num);
    if (existing_node != NULL) {
        return existing_node;
    }

    // Calculate buffer position
    void *buffer = (char*)db->data + (page_num * PAGE_SIZE);
    
    // Allocate and initialize node
    RowNode* node = (RowNode*)calloc(1, sizeof(RowNode));
    if (!node) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }
    
    // Deserialize node data
    size_t offset = 0;
    memcpy(&node->is_leaf, buffer + offset, sizeof(node->is_leaf));
    offset += sizeof(node->is_leaf);
    
    memcpy(&node->num_keys, buffer + offset, sizeof(node->num_keys));
    offset += sizeof(node->num_keys);
    
    // Validate num_keys
    if (node->num_keys > 169) {
        free(node);
        return NULL;
    }
    
    memcpy(&node->page_num, buffer + offset, sizeof(node->page_num));
    offset += sizeof(node->page_num);
    
    memcpy(node->keys, buffer + offset, sizeof(node->keys));
    offset += sizeof(node->keys);
    
    memcpy(node->raw_data, buffer + offset, sizeof(node->raw_data));
    offset += sizeof(node->raw_data);
    
    memcpy(node->link, buffer + offset, sizeof(node->link));
    
    // Register this node as visited before loading children
    add_visited_node(page_num, node);
    
    // Load child nodes
    if (!node->is_leaf) {
        for (int i = 0; i <= node->num_keys; i++) {
            if (node->link[i] != 0) {
                node->plink[i] = deserialize_node(db, node->link[i]);
            } else {
                node->plink[i] = NULL;
            }
        }
    }

    return node;
}

// Funcție modificată pentru deserializarea întregului arbore B
RowNode* deserialize_btree(DBFile* db) {
    // Resetează array-ul de noduri vizitate
    visited_count = 0;
    
    // Verifică dacă fișierul este valid
    if (db->data == NULL || db->size < sizeof(uint64_t)) {
        printf("Error: Invalid database file\n");
        return NULL;
    }
    
    uint64_t root_page_num;

    // Citește root_page_num din pagina 0
    memcpy(&root_page_num, db->data, sizeof(root_page_num));
    printf("Deserialized root from page %lu\n", root_page_num);
    
    // Verifică dacă root_page_num este valid
    if (root_page_num == 0 || root_page_num * PAGE_SIZE >= db->size) {
        printf("Error: Invalid root page number: %lu\n", root_page_num);
        return NULL;
    }
    
    // Încarcă arborele
    return deserialize_node(db, root_page_num);
}

// Funcția principală pentru încărcarea arborelui de pe disc
RowNode* load_btree_from_disk(DBFile* db) {
    printf("Loading B-tree from disk...\n");
    RowNode* loaded_root = deserialize_btree(db);
    
    if (loaded_root == NULL) {
        printf("Failed to load B-tree from disk\n");
    } else {
        printf("Successfully loaded B-tree with root at page %lu\n", loaded_root->page_num);
        printf("Total nodes loaded: %d\n", visited_count);
    }
    
    return loaded_root;
}

// Functions for deletion

// Find the in-order predecessor (largest value in left subtree)
int find_predecessor(const struct RowNode *node) {
    // Start at the given node
    const struct RowNode *current = node;
    
    // If the node is a leaf, return its rightmost key
    if (current->is_leaf) {
        return current->keys[current->num_keys];
    }
    
    // Otherwise, find the rightmost leaf node in the subtree
    while (!current->is_leaf) {
        current = current->plink[current->num_keys];
    }
    
    return current->keys[current->num_keys];
}

// Find the in-order successor (smallest value in right subtree)
int find_successor(const struct RowNode *node) {
    // Start at the given node
    const struct RowNode *current = node;
    
    // If the node is a leaf, return its leftmost key
    if (current->is_leaf) {
        return current->keys[1];
    }
    
    // Otherwise, find the leftmost leaf node in the subtree
    while (!current->is_leaf) {
        current = current->plink[0];
    }
    
    return current->keys[1];
}

// Shift values and child pointers left after deletion
void shift_left(struct RowNode *node, const int pos) {
    for (int i = pos; i < node->num_keys; i++) {
        node->keys[i] = node->keys[i + 1];
        node->plink[i] = node->plink[i + 1];
        node->link[i] = node->link[i + 1];
    }
    
    node->plink[node->num_keys] = node->plink[node->num_keys + 1];
    node->link[node->num_keys] = node->link[node->num_keys + 1];
    node->num_keys--;
}

// Helper function to check if a node can spare a key (has more than minimum keys)
int can_spare_key(struct RowNode *node) {
    return node && node->num_keys > ROW_MIN_KEYS;
}

// Function to handle the case when a node becomes underfull
void handle_underfull_node(struct RowNode *parent, int idx) {
    struct RowNode *current = parent->plink[idx];
    
    // Try borrowing from left sibling
    if (idx > 0 && can_spare_key(parent->plink[idx-1])) {
        struct RowNode *left_sibling = parent->plink[idx-1];
        
        // Shift all keys and children in current node to the right
        for (int i = current->num_keys; i > 0; i--) {
            current->keys[i+1] = current->keys[i];
            current->plink[i+1] = current->plink[i];
            current->link[i+1] = current->link[i];
        }
        current->plink[1] = current->plink[0];
        current->link[1] = current->link[0];
        
        // Move key from parent to current
        current->keys[1] = parent->keys[idx];
        
        // Move rightmost key from left sibling to parent
        parent->keys[idx] = left_sibling->keys[left_sibling->num_keys];
        
        // Move rightmost child from left sibling to current
        current->plink[0] = left_sibling->plink[left_sibling->num_keys];
        if (current->plink[0]) {
            current->link[0] = current->plink[0]->page_num;
        }
        
        // Update key counts
        current->num_keys++;
        left_sibling->num_keys--;
        return;
    }
    
    // Try borrowing from right sibling
    if (idx < parent->num_keys && can_spare_key(parent->plink[idx+1])) {
        struct RowNode *right_sibling = parent->plink[idx+1];
        
        // Move key from parent to current
        current->keys[current->num_keys+1] = parent->keys[idx+1];
        
        // Move leftmost child from right sibling to current
        current->plink[current->num_keys+1] = right_sibling->plink[0];
        if (current->plink[current->num_keys+1]) {
            current->link[current->num_keys+1] = current->plink[current->num_keys+1]->page_num;
        }
        
        // Move leftmost key from right sibling to parent
        parent->keys[idx+1] = right_sibling->keys[1];
        
        // Shift all keys and children in right sibling to the left
        for (int i = 1; i < right_sibling->num_keys; i++) {
            right_sibling->keys[i] = right_sibling->keys[i+1];
            right_sibling->plink[i-1] = right_sibling->plink[i];
            right_sibling->link[i-1] = right_sibling->link[i];
        }
        right_sibling->plink[right_sibling->num_keys-1] = right_sibling->plink[right_sibling->num_keys];
        right_sibling->link[right_sibling->num_keys-1] = right_sibling->link[right_sibling->num_keys];
        
        // Update key counts
        current->num_keys++;
        right_sibling->num_keys--;
        return;
    }
    
    // If can't borrow, merge with a sibling
    if (idx > 0) {
        // Merge with left sibling
        struct RowNode *left_sibling = parent->plink[idx-1];
        
        // Move key from parent to left sibling
        left_sibling->keys[left_sibling->num_keys+1] = parent->keys[idx];
        
        // Move all keys and children from current to left sibling
        for (int i = 1; i <= current->num_keys; i++) {
            left_sibling->keys[left_sibling->num_keys+i+1] = current->keys[i];
            left_sibling->plink[left_sibling->num_keys+i] = current->plink[i-1];
            left_sibling->link[left_sibling->num_keys+i] = current->link[i-1];
        }
        left_sibling->plink[left_sibling->num_keys+current->num_keys+1] = current->plink[current->num_keys];
        left_sibling->link[left_sibling->num_keys+current->num_keys+1] = current->link[current->num_keys];
        
        // Update key count
        left_sibling->num_keys += current->num_keys + 1;
        
        // Remove parent key and update parent's child pointer
        shift_left(parent, idx);
        
        // Free memory and return page to free queue
        push(free_page_queue, current->page_num);
        free(current);
    } else {
        // Merge with right sibling
        struct RowNode *right_sibling = parent->plink[idx+1];
        
        // Move key from parent to current
        current->keys[current->num_keys+1] = parent->keys[idx+1];
        
        // Move all keys and children from right sibling to current
        for (int i = 1; i <= right_sibling->num_keys; i++) {
            current->keys[current->num_keys+i+1] = right_sibling->keys[i];
            current->plink[current->num_keys+i] = right_sibling->plink[i-1];
            current->link[current->num_keys+i] = right_sibling->link[i-1];
        }
        current->plink[current->num_keys+right_sibling->num_keys+1] = right_sibling->plink[right_sibling->num_keys];
        current->link[current->num_keys+right_sibling->num_keys+1] = right_sibling->link[right_sibling->num_keys];
        
        // Update key count
        current->num_keys += right_sibling->num_keys + 1;
        
        // Remove parent key and update parent's child pointer
        shift_left(parent, idx+1);
        
        // Free memory and return page to free queue
        push(free_page_queue, right_sibling->page_num);
        free(right_sibling);
    }
}

// Function to delete a value from a node
void delete_value(struct RowNode *node, const int val) {
    int pos;

    // Find position of value in current node
    if (val < node->keys[1]) {
        pos = 0;
    } else {
        for (pos = node->num_keys; (val < node->keys[pos] && pos > 1); pos--);
    }

    // If value is found in this node
    if (pos <= node->num_keys && val == node->keys[pos]) {
        // Case 1: If node is a leaf, simply remove the value
        if (node->is_leaf) {
            shift_left(node, pos);
            if (node == root && node->num_keys == 0) {
                // Tree is now empty
                push(free_page_queue, node->page_num);
                free(node);
                root = NULL;
                return;
            }
            printf("Value %d deleted from the tree\n", val);
            return;
        } 
        // Case 2: If node is an internal node
        else {
            // Replace with predecessor or successor
            if (node->plink[pos-1] != NULL) {
                // Find the predecessor from left subtree
                const int pred = find_predecessor(node->plink[pos-1]);
                node->keys[pos] = pred;
                delete_value(node->plink[pos-1], pred);
                
                // Check if child became underfull
                if (node->plink[pos-1]->num_keys < ROW_MIN_KEYS) {
                    handle_underfull_node(node, pos-1);
                }
            } else {
                // Find the successor from right subtree
                const int succ = find_successor(node->plink[pos]);
                node->keys[pos] = succ;
                delete_value(node->plink[pos], succ);
                
                // Check if child became underfull
                if (node->plink[pos]->num_keys < ROW_MIN_KEYS) {
                    handle_underfull_node(node, pos);
                }
            }
        }
    } 
    // If value is not found in this node, search in the correct subtree
    else {
        // If we've reached a leaf without finding the value, it's not in the tree
        if (node->is_leaf) {
            printf("Value %d not found in the tree\n", val);
            return;
        }
        
        // Recursively delete from the appropriate child
        delete_value(node->plink[pos], val);
        
        // Check if child became underfull
        if (node->plink[pos]->num_keys < ROW_MIN_KEYS) {
            handle_underfull_node(node, pos);
        }
    }
}

// Function to delete a value from the B-tree
void delete_value_from_tree(const int val) {
    if (root == NULL) {
        printf("Tree is empty\n");
        return;
    }

    delete_value(root, val);

    // If root has only one child left, make that child the new root
    if (root->num_keys == 0 && !root->is_leaf) {
        struct RowNode *temp = root;
        root = root->plink[0];
        push(free_page_queue, temp->page_num);
        free(temp);
    }
}

// Function to insert a value into the B-tree
void insert(const int val) {
    int i;
    struct RowNode *child;

    const int flag = set_value(val, &i, root, &child);
    
    // If root was split or it's the first insertion, create new root
    if (flag) {
        struct RowNode *new_root = create_node(i, child);
        new_root->is_leaf = 0;  // New root with children is not a leaf
        root = new_root;
    }

    printf("Inserted value %d, root page: %lu\n", val, root->page_num);
}