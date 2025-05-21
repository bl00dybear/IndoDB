#include "../../include/core/row_btree_ops.h"
#include "../../include/config.h"
#include "../../include/utils//queue.h"
#include "../../include/core//memory_ops.h"


RowNode *create_node(const uint64_t key,void *data, RowNode *child) {
    RowNode *newNode = malloc(sizeof(struct RowNode));
    if (!newNode) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }
    
    // Initialize the node
    memset(newNode, 0, sizeof(struct RowNode));
    newNode->keys[1] = key;
    newNode->raw_data[1] = data;
    newNode->num_keys = 1;
    for (int i = 0; i < ROW_MAX_KEYS + 1; i++) {
    newNode->link[i] = 0;  // Initialize all links to 0
    }
    newNode->link[0] = root ? root->page_num : 0;
    newNode->link[1] = child ? child->page_num : 0;
    // Set links
    newNode->plink[0] = root;
    newNode->plink[1] = child;
    if(is_empty(free_page_queue)) {
        newNode->page_num = 1;    
    }
    else{
    // Allocate page number from the free page queue
    newNode->page_num = front(free_page_queue);
    pop(free_page_queue);
    }
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
void insert_node(const uint64_t key, void* data,const int pos, RowNode *node, RowNode *child) {
    int j = node->num_keys;

    // Shift values and child pointers to make space for the new value
    while (j > pos) {
        node->keys[j + 1] = node->keys[j];
        node->raw_data[j + 1] = node->raw_data[j];
        node->plink[j + 1] = node->plink[j];
        node->link[j + 1] = node->link[j];
        j -= 1;
    }

    // Insert value at the correct position
    node->keys[j + 1] = key;
    node->raw_data[j + 1] = data;
    node->plink[j + 1] = child;

    // TODO: verify link here
    
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
void split_node(const uint64_t key,void *data, int *pval, const int pos, RowNode *node, struct RowNode *child, RowNode **newNode) {
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
    memset(*newNode, 0, sizeof(RowNode));
    
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
        insert_node(key,data, pos, node, child);
    } else {
        insert_node(key, data,pos - median, *newNode, child);
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
int set_value(const uint64_t key, void* data, int *pval, struct RowNode *node, struct RowNode **child) {
    int pos;

    // If a tree is empty, or we've reached a leaf, create a new node
    if (!node) {
        *pval = key;
        *child = NULL;
        return 1;
    }

    // Finding the correct position for the value in the current node
    if (key < node->keys[1]) {
        pos = 0;
    } else {
        for (pos = node->num_keys; (key < node->keys[pos] && pos > 1); pos--);

        // Prevent duplicate values (optional)
        // if (val == node->keys[pos]) {
        //     printf("Duplicates are not permitted\n");
        //     return 0;
        // }
    }

    // If the node is a leaf, insert directly
    if (node->is_leaf) {
        if (node->num_keys < ROW_MAX_KEYS) {
            insert_node(key,data, pos, node, NULL);
            return 0;
        } else {
            // Split leaf node
            split_node(key,data, pval, pos, node, NULL, child);
            return 1;
        }
    }
    // For internal nodes, recursively insert into the correct subtree
    if (set_value(key,data, pval, node->plink[pos], child)) {
        // If the child was split, insert the promoted value
        if (node->num_keys < ROW_MAX_KEYS) {
            insert_node(*pval,data, pos, node, *child);
            return 0;
        } else {
            // Split internal node
            split_node(*pval,data, pval, pos, node, *child, child);
            return 1;
        }
    }
    return 0;
}

// Function to search for a value in the B-tree
void search(const uint64_t key, int *pos, RowNode *myNode) {
    if (!myNode) {
        printf("%lu not found in the tree\n", key);
        return;
    }

    // Find the correct position
    if (key < myNode->keys[1]) {
        *pos = 0;
    } else {
        for (*pos = myNode->num_keys; (key < myNode->keys[*pos] && *pos > 1); (*pos)--)
            ;

        // If found, print a message and return
        if (key == myNode->keys[*pos]) {
            printf("%lu is found\n", key);
            return;
        }
    }
    
    // If this is a leaf, and we didn't find the value, it's not in the tree
    if (myNode->is_leaf) {
        printf("%lu not found in the tree\n", key);
        return;
    }
    
    // Recursively search in the correct subtree
    search(key, pos, myNode->plink[*pos]);
}

// Function to perform an in-order traversal of the B-tree
void traversal(const struct RowNode *myNode) {
    if (myNode == NULL) {
        printf("Node is NULL\n");
        return;
    }
    
    // printf("Node at page %lu, keys: %d, is_leaf: %s\n", 
    //        myNode->page_num, myNode->num_keys, 
    //        myNode->is_leaf ? "Yes" : "No");
    
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
            // printf("Traversing to child %d (page %lu):\n", i, myNode->plink[i]->page_num);
            traversal(myNode->plink[i]);
        }
    }
}
// Serialization functions

// Function to find an already visited node
RowNode* find_visited_node(uint64_t page_num) {
    for (int i = 0; i < visited_count; i++) {
        if (visited_nodes[i].page_num == page_num) {
            return visited_nodes[i].node;
        }
    }
    return NULL;
}

// Function to add a node to the list of visited nodes
void add_visited_node(uint64_t page_num, RowNode* node) {
    if (visited_count < MAX_VISITED_NODES) {
        visited_nodes[visited_count].page_num = page_num;
        visited_nodes[visited_count].node = node;
        visited_count++;
    } else {
        printf("Warning: Visited nodes array is full, can't track more nodes\n");
    }
}


// Function to check if a page has already been serialized
bool is_page_serialized(uint64_t page_num) {
    for (int i = 0; i < serialized_count; i++) {
        if (serialized_pages[i] == page_num) {
            return true;
        }
    }
    return false;
}

// Function to add a page to the list of serialized pages
void add_serialized_page(uint64_t page_num) {
    if (serialized_count < MAX_VISITED_NODES) {
        serialized_pages[serialized_count++] = page_num;
    }
}

void serialize_metadata(DBFile* db, MetadataPage* metadata) {
    if (!db || !metadata) {
        printf("Error: Invalid database or metadata\n");
        return;
    }
    
    // Update free page bitmap from queue
    memset(metadata->free_page_bitmap, 0xFF, sizeof(metadata->free_page_bitmap));
    
    QueueNode* current = free_page_queue->front;
    while (current != NULL) {
        uint64_t page_num = current->data;
        size_t byte_index = page_num / 8;
        uint8_t bit_pos = page_num % 8;
        metadata->free_page_bitmap[byte_index] &= ~(1 << bit_pos);
        current = current->next;
    }
    
    // Write entire metadata page
    memcpy(db->data, metadata, METADATA_SIZE);
}

void deserialize_metadata(DBFile* db, MetadataPage* metadata) {
    if (!db || !db->data || !metadata) {
        printf("Error: Invalid database or metadata\n");
        return;
    }
    
    // Read entire metadata page
    memcpy(metadata, db->data, METADATA_SIZE);
    
    // Reconstruct free page queue from bitmap
    if (free_page_queue) {
        destroy_queue(free_page_queue);
    }
    free_page_queue = create_queue();
    
    for (uint64_t page_num = 1; page_num < sizeof(metadata->free_page_bitmap) * 8; page_num++) {
        size_t byte_index = page_num / 8;
        uint8_t bit_pos = page_num % 8;
        if (metadata->free_page_bitmap[byte_index] & (1 << bit_pos)) {
            push(free_page_queue, page_num);
        }
    }
    global_id= metadata->last_table_id;
    if(metadata->magic != MAGIC_NUMBER) {
        printf("Error: corrupted database\n");
        exit(EXIT_FAILURE);
    }
}

void serialize_free_page_queue(DBFile* db) {
    if (!db || !free_page_queue) {
        printf("Error: Invalid database or queue\n");
        return;
    }

    size_t offset = sizeof(uint64_t); // Skip root page number
    unsigned char* bitmap = calloc(PAGE_SIZE - offset, 1);

    // Set all bits to 1 (marking all pages as free)
    memset(bitmap, 0xFF, PAGE_SIZE - offset);

    // Mark used pages (set bits to 0)
    QueueNode* current = free_page_queue->front;
    while (current != NULL) {
        uint64_t page_num = current->data;
        // Calculate byte and bit position
        size_t byte_index = page_num / 8;
        uint8_t bit_pos = page_num % 8;
        // Set bit to 0 (page is used)
        bitmap[byte_index] &= ~(1 << bit_pos);
        current = current->next;
    }

    // Write bitmap to metadata page
    memcpy((char*)db->data + offset, bitmap, PAGE_SIZE - offset);
    free(bitmap);
}


void deserialize_free_page_queue(DBFile* db) {
    if (!db || !db->data) {
        printf("Error: Invalid database\n");
        return;
    }

    size_t offset = sizeof(uint64_t); // Skip root page number
    unsigned char* bitmap = (unsigned char*)((char*)db->data + offset);

    // Create new queue
    Queue* new_queue = create_queue();

    // Read bitmap and reconstruct queue
    for (uint64_t page_num = 1; page_num < (PAGE_SIZE - offset) * 8; page_num++) {
        size_t byte_index = page_num / 8;
        uint8_t bit_pos = page_num % 8;

        // If bit is 1, page is free
        if (bitmap[byte_index] & (1 << bit_pos)) {
            push(new_queue, page_num);
        }
    }

    // Replace old queue with new one
    if (free_page_queue) {
        destroy_queue(free_page_queue);
    }
    free_page_queue = new_queue;
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
            // printf("%ld %ld\n",node->page_num, node->link[i]);
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

// Modified function for serializing the entire B tree
void serialize_btree(DBFile* db, RowNode* root, MetadataPage* metadata) {
    if (!root) {
        // printf("Error: Cannot serialize NULL root\n");
        return;
    }

    // Reset the array of serialized pages
    serialized_count = 0;

    // Save the root page number in metadata (page 0)
    metadata->root_page_num = root->page_num;
    metadata->last_table_id = global_id;
    metadata->magic = MAGIC_NUMBER;
    // Serialize the rest of the tree
    serialize_node(db, root);
    serialize_metadata(db, metadata);
    
    // printf("Serialization complete. Total nodes serialized: %d\n", serialized_count);
}

// Node deserialization function, modified to use the list of visited nodes
RowNode* deserialize_node(DBFile* db, uint64_t page_num) {
    // Validate page number
    if (page_num == 0 || page_num * PAGE_SIZE >= (uint64_t)db->size) {
        return NULL;
    }
    
    // Check if a node is already loaded
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

// Modified function for deserializing the entire B-tree
RowNode* deserialize_btree(DBFile* db, MetadataPage* metadata) {
    // Reset the array of visited nodes
    visited_count = 0;

    // Check if the file is valid
    if (db->data == NULL || (uint64_t)db->size < sizeof(uint64_t)) {
        printf("Error: Invalid database file\n");
        return NULL;
    }
    
    deserialize_metadata(db, metadata);

    // Check if root_page_num is valid
    if (metadata->root_page_num == 0 || metadata->root_page_num * PAGE_SIZE >= (uint64_t)db->size) {
        printf("Error: Invalid root page number: %lu\n", metadata->root_page_num);
        return NULL;
    }

    // Tree load
    return deserialize_node(db, metadata->root_page_num);
}

// Main function for loading the tree from disk
RowNode* load_btree_from_disk(DBFile* db, MetadataPage* metadata) {
    if (!db || !metadata) {
        printf("Error: Invalid database or metadata\n");
        return NULL;
    }
    // printf("Loading B-tree from disk...\n");
    RowNode* loaded_root = deserialize_btree(db,metadata);
    
    // if (loaded_root == NULL) {
    //     printf("Failed to load B-tree from disk\n");
    // } else {
    //     printf("Successfully loaded B-tree with root at page %lu\n", loaded_root->page_num);
    //     printf("Table name: %s\n", metadata->table_name);
    //     printf("Number of columns: %d\n", metadata->num_columns);
    //     printf("Total nodes loaded: %d\n", visited_count);
        
    // }
    
    return loaded_root;
}

/*
                            Functions for deletion
*/

// Find the in-order predecessor (the largest value in the left subtree)
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

// Find the in-order successor (the smallest value in the right subtree)
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
int can_spare_key(const struct RowNode *node) {
    return node && node->num_keys > ROW_MIN_KEYS;
}

// Function to handle the case when a node becomes underfull
void handle_underfull_node(struct RowNode *parent, int idx) {
    struct RowNode *current = parent->plink[idx];
    
    // Try borrowing from the left sibling
    if (idx > 0 && can_spare_key(parent->plink[idx-1])) {
        struct RowNode *left_sibling = parent->plink[idx-1];
        
        // Shift all keys and children in the current node to the right
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
    
    // Try borrowing from the right sibling
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
        
        // Shift all keys and children in the right sibling to the left
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
    
    // If you can't borrow, merge with a sibling
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
        
        // Remove the parent key and update the parent's child pointer
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
        
        // Remove the parent key and update the parent's child pointer
        shift_left(parent, idx+1);
        
        // Free memory and return page to free queue
        push(free_page_queue, right_sibling->page_num);
        free(right_sibling);
    }
}

// Function to delete a value from a node
void delete_value(struct RowNode *node, const uint64_t key) {
    int pos;

    // Find position of value in current node
    if (key < node->keys[1]) {
        pos = 0;
    } else {
        for (pos = node->num_keys; (key < node->keys[pos] && pos > 1); pos--);
    }

    // If value is found in this node
    if (pos <= node->num_keys && key == node->keys[pos]) {
        // Case 1: If a node is a leaf, remove the value
        if (node->is_leaf) {
            shift_left(node, pos);
            if (node == root && node->num_keys == 0) {
                // Tree is now empty
                push(free_page_queue, node->page_num);
                free(node);
                root = NULL;
                return;
            }
            printf("Value %lu deleted from the tree\n", key);
            return;
        } 
        // Case 2: If a node is an internal node
        else {
            // Replace it with a predecessor or successor
            if (node->plink[pos-1] != NULL) {
                // Find the predecessor from the left subtree
                const int pred = find_predecessor(node->plink[pos-1]);
                node->keys[pos] = pred;
                delete_value(node->plink[pos-1], pred);
                
                // Check if the child became underfull
                if (node->plink[pos-1]->num_keys < ROW_MIN_KEYS) {
                    handle_underfull_node(node, pos-1);
                }
            } else {
                // Find the successor from the right subtree
                const int succ = find_successor(node->plink[pos]);
                node->keys[pos] = succ;
                delete_value(node->plink[pos], succ);
                
                // Check if the child became underfull
                if (node->plink[pos]->num_keys < ROW_MIN_KEYS) {
                    handle_underfull_node(node, pos);
                }
            }
        }
    } 
    // If a value is not found in this node, search in the correct subtree
    else {
        // If we've reached a leaf without finding the value, it's not in the tree
        if (node->is_leaf) {
            printf("Value %lu not found in the tree\n", key);
            return;
        }
        
        // Recursively delete it from the appropriate child
        delete_value(node->plink[pos], key);

        // Check if the child became underfull
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

    // If the root has only one child left, make that child the new root
    if (root->num_keys == 0 && !root->is_leaf) {
        RowNode *temp = root;
        root = root->plink[0];
        push(free_page_queue, temp->page_num);
        free(temp);
    }
}

void insert(const uint64_t key, void* data) {
    int i;
    RowNode *child;

    const int flag = set_value(key,data, &i, root, &child);
    
    // If the root was split, or it's the first insertion, create a new root
    if (flag) {
        RowNode *new_root = create_node(i,data, child);
        new_root->is_leaf = 0;  // New root with children is not a leaf
        root = new_root;
    }

    // printf("Inserted value %ld, root page: %lu\n", key, root->page_numAA);
}