#ifndef ROW_BTREE_OP_H
#define ROW_BTREE_OP_H

#include "libraries.h"
#include "data_structures.h"


struct RowNode *create_node(const int val, struct RowNode *child);
void write_on_memory_block(DBFile *db, const void* new_data, u_int64_t page_num);
void insert_node(const int val, const int pos, struct RowNode *node, struct RowNode *child);
void split_node(const int val, int *pval, const int pos, struct RowNode *node, struct RowNode *child, struct RowNode **newNode) ;
int set_value(const int val, int *pval, struct RowNode *node, struct RowNode **child) ;
void search(const int val, int *pos, struct RowNode *myNode) ;
void traversal(const struct RowNode *myNode) ;
RowNode* find_visited_node(uint64_t page_num) ;
void add_visited_node(uint64_t page_num, RowNode* node) ;
bool is_page_serialized(uint64_t page_num) ;
void add_serialized_page(uint64_t page_num) ;
void serialize_node(DBFile* db, RowNode* node) ;
void serialize_btree(DBFile* db, RowNode* root) ;
RowNode* deserialize_node(DBFile* db, uint64_t page_num) ;
RowNode* deserialize_btree(DBFile* db) ;
RowNode* load_btree_from_disk(DBFile* db) ;
int find_predecessor(const struct RowNode *node) ;
int find_successor(const struct RowNode *node) ;
void shift_left(struct RowNode *node, const int pos) ;
int can_spare_key(struct RowNode *node) ;
void handle_underfull_node(struct RowNode *parent, int idx) ;
void delete_value(struct RowNode *node, const int val) ;
void delete_value_from_tree(const int val) ;
void insert(const int val) ;


#endif
