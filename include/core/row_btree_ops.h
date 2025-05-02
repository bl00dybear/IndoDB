#ifndef ROW_BTREE_OP_H
#define ROW_BTREE_OP_H

#include "../libraries.h"
#include "../data/core_structures.h"


/*
 * B-tree Insert Function Flow
 *
 * 1. Initial Function Call:
 *    - When insert(current_id) is called from cli_interactions(), it receives the global_id value
 *    - The function likely starts by checking if the root node exists
 *
 * 2. Empty Tree Handling:
 *    - If root == NULL (first insertion):
 *      a. Allocate a new node using a page number from free_page_queue
 *      b. Set the node as leaf (is_leaf = 1)
 *      c. Initialize node (num_keys = 0, page_num = popped value from queue)
 *      d. Insert the key in the first position (index 0)
 *      e. Increment num_keys
 *      f. Set this node as the root
 *      g. Function returns
 *
 * 3. Non-Empty Tree Handling:
 *    - If root already exists:
 *      a. Check if the root is full (num_keys == ROW_MAX_KEYS)
 *      b. If the root is full, the tree needs to grow in height:
 *         - Create a new root node
 *         - Make the old root a child of the new root
 *         - Split the old root and adjust key distribution
 *      c. Call the recursive insert_node() function starting at root
 *
 * 4. Recursive Insert Function (insert_node):
 *    - Parameters: key to insert, node to start from
 *    - If the node is a leaf:
 *      a. Find the correct position for the new key (maintaining sorted order)
 *      b. Shift all greater keys and pointers to make room
 *      c. Insert the key in the correct position
 *      d. Increment num_keys
 *      e. Return
 *
 * 5. Node Splitting Process:
 *    - If a node is full (num_keys == ROW_MAX_KEYS) and needs insertion:
 *      a. Create a new node with a page number from free_page_queue
 *      b. Determine median key position
 *      c. Move all keys and pointers greater than median to the new node
 *      d. If the node is a leaf, adjust links to maintain the leaf chain
 *      e. Insert the median key into the parent node (may cause recursive splits)
 *      f. Update all parent and child pointers to maintain tree structure
 *
 * 6. Internal Node Traversal:
 *    - If the node is not a leaf:
 *      a. Find the correct child pointer to follow based on key comparison
 *      b. If that child node is full, split it before going down
 *      c. Recursively call insert_node on the appropriate child
 *
 * 7. Visited Node Tracking:
 *    - During insertion, the algorithm likely adds nodes to visited_nodes array
 *    - This helps prevent cycles and enables proper cleanup and serialization
 *
 * 8. Memory Management:
 *    - All new nodes get page numbers from free_page_queue
 *    - When nodes are deleted or restructured, their page numbers are returned to queue
 *
 * 9. Completion:
 *    - Once insert operation is complete, the database file is marked as dirty
 *    - Changes will be persisted when commit_changes() is called
 */

RowNode *create_node(const uint64_t key, void *data, RowNode *child);
void write_on_memory_block(DBFile *db, void* new_data, u_int64_t page_num);
void insert_node(const uint64_t key, void* data, const int pos, RowNode *node, RowNode *child);
void split_node(const uint64_t key, void *data, int *pval, const int pos, struct RowNode *node, struct RowNode *child, struct RowNode **newNode);
int set_value(const uint64_t key, void* data, int *pval, struct RowNode *node, struct RowNode **child);
void search(const int val, int *pos, RowNode *myNode);
void traversal(const struct RowNode *myNode);
RowNode* find_visited_node(uint64_t page_num);
void add_visited_node(uint64_t page_num, RowNode* node);
bool is_page_serialized(uint64_t page_num);
void add_serialized_page(uint64_t page_num);
void serialize_node(DBFile* db, RowNode* node);
void serialize_btree(DBFile* db, RowNode* root);
RowNode* deserialize_node(DBFile* db, uint64_t page_num);
RowNode* deserialize_btree(DBFile* db);
RowNode* load_btree_from_disk(DBFile* db);
int find_predecessor(const struct RowNode *node);
int find_successor(const struct RowNode *node);
void shift_left(struct RowNode *node, const int pos);
int can_spare_key(const struct RowNode *node);
void handle_underfull_node(struct RowNode *parent, int idx);
void delete_value(struct RowNode *node, const int val);
void delete_value_from_tree(const int val);
void insert(const uint64_t key, void* data);


#endif
