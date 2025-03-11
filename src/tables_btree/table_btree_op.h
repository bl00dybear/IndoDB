#include "data_structures.h"

TableNode *create_node(u_int8_t is_leaf) {
    TableNode *node = (TableNode *)malloc(sizeof(TableNode));
    node->is_leaf = is_leaf;
    node->num_keys = 0;
    memset(node->root_page, 0, sizeof(node->root_page));
    memset(node->keys, 0, sizeof(node->keys));
    memset(node->link, 0, sizeof(node->link));
    return node;
}

int find_position(TableNode *node, const char *key) {
    int pos = 0;
    while (pos < node->num_keys && strcmp(node->keys[pos], key) < 0) {
        pos++;
    }
    return pos;
}

void insert_non_full(TableNode *node, const char *key, u_int16_t root_page, u_int64_t child_link) {
    int pos = find_position(node, key);
    for (int i = node->num_keys; i > pos; i--) {
        strcpy(node->keys[i], node->keys[i - 1]);
        node->root_page[i] = node->root_page[i - 1];
        node->link[i + 1] = node->link[i];
    }
    strcpy(node->keys[pos], key);
    node->root_page[pos] = root_page;
    node->link[pos + 1] = child_link;
    node->num_keys++;
}

void split_child(TableNode *parent, int index, TableNode *child) {
    TableNode *new_child = create_node(child->is_leaf);
    new_child->num_keys = TABLE_MAX_KEYS / 2;

    for (int i = 0; i < TABLE_MAX_KEYS / 2; i++) {
        strcpy(new_child->keys[i], child->keys[i + TABLE_MAX_KEYS / 2 + 1]);
        new_child->root_page[i] = child->root_page[i + TABLE_MAX_KEYS / 2 + 1];
    }
    if (!child->is_leaf) {
        for (int i = 0; i <= TABLE_MAX_KEYS / 2; i++) {
            new_child->link[i] = child->link[i + TABLE_MAX_KEYS / 2 + 1];
        }
    }

    child->num_keys = TABLE_MAX_KEYS / 2;

    for (int i = parent->num_keys; i > index; i--) {
        strcpy(parent->keys[i], parent->keys[i - 1]);
        parent->root_page[i] = parent->root_page[i - 1];
        parent->link[i + 1] = parent->link[i];
    }
    strcpy(parent->keys[index], child->keys[TABLE_MAX_KEYS / 2]);
    parent->root_page[index] = child->root_page[TABLE_MAX_KEYS / 2];
    parent->link[index + 1] = (u_int64_t)new_child;
    parent->num_keys++;
}

void insert(TableNode **root, const char *key, u_int16_t root_page) {
    TableNode *r = *root;
    if (r->num_keys == TABLE_MAX_KEYS) {
        TableNode *s = create_node(0);
        s->link[0] = (u_int64_t)r;
        split_child(s, 0, r);
        *root = s;
    }
    TableNode *current = *root;
    while (!current->is_leaf) {
        int i = find_position(current, key);
        TableNode *child = (TableNode *)current->link[i];
        if (child->num_keys == TABLE_MAX_KEYS) {
            split_child(current, i, child);
            if (strcmp(key, current->keys[i]) > 0) {
                i++;
            }
        }
        current = (TableNode *)current->link[i];
    }
    insert_non_full(current, key, root_page, 0);
}

int search(TableNode *node, const char *key) {
    int i = find_position(node, key);
    if (i < node->num_keys && strcmp(node->keys[i], key) == 0) {
        return 1;
    }
    if (node->is_leaf) {
        return 0;
    } else {
        return search((TableNode *)node->link[i], key);
    }
}

void traversal(TableNode *node) {
    if (!node) return;
    for (int i = 0; i < node->num_keys; i++) {
        traversal((TableNode *)node->link[i]);
        printf("%s ", node->keys[i]);
    }
    traversal((TableNode *)node->link[node->num_keys]);
}