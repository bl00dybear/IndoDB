#ifndef BF5ED1DD_D1BD_4342_B75A_C25BB21B57A4
#define BF5ED1DD_D1BD_4342_B75A_C25BB21B57A4

// Function to create a new node with a given value
// Parameters:
// - val: The value to insert
// - child: Pointer to the right child node

#include "data_structures.h"
#include "config.h"

struct RowNode *create_node(const int val, struct RowNode *child) {
    struct RowNode *newNode = (struct RowNode *)malloc(sizeof(struct RowNode));
    newNode->keys[1] = val;
    newNode->num_keys = 1;
    newNode->plink[0] = root;
    newNode->plink[1] = child;//la root aici practic il pune pe NULL
    newNode->page_num = 1;
    // newNode->link[0] = root->page_num;
    // newNode->link[1] = child->page_num;
    return newNode;
}


// Function to insert a value into a node at the given position
// Parameters:
// - val: The value to insert
// - pos: The position where value should be inserted
// - node: Pointer to the node in which value will be inserted
// - child: Pointer to the child node associated with the value
void insert_node(const int val, const int pos, struct RowNode *node, struct RowNode *child) {
    int j = node->num_keys;

    // Shift values and child pointers to make space for the new value
    while (j > pos) {
        node->keys[j + 1] = node->keys[j];
        node->plink[j + 1] = node->plink[j];
        node->link[j + 1] = node->link[j];
        j -= 1;
    }

    node->keys[j + 1] = val;  // Insert value at correct position
    node->plink[j + 1] = child;  // Adjust child plink
    // node->link[j + 1] = child->page_num;  // Adjust child link
    node->num_keys += 1;  // Increment num_keys of values
}

// Function to split a node when it exceeds the maximum limit
// Parameters:
// - val: The value to insert
// - pval: Pointer to store the value that will move up
// - pos: Position of insertion in the current node
// - node: Pointer to the node to be split
// - child: Pointer to the child node associated with val
// - newNode: Pointer to the newly created node
void split_node(const int val, int *pval, const int pos, struct RowNode *node, struct RowNode *child, struct RowNode **newNode) {
    int median;

    // Determine the median index to split
    if (pos > ROW_MIN_KEYS)
        median = ROW_MIN_KEYS + 1;
    else
        median = ROW_MIN_KEYS;

    // Allocate memory for the new right node
    *newNode = (struct RowNode *)malloc(sizeof(struct RowNode));
    int j = median + 1;

    // o functie care returneaza numarul unei pagini libere
    // pentru a o completa in page_num
    // (*newNode)->page_num = get_free_page_num();

    // Move values and child pointers from the original node to the new node
    while (j <= ROW_MAX_KEYS) {
        (*newNode)->keys[j - median] = node->keys[j];
        (*newNode)->plink[j - median] = node->plink[j];
        //aici trebuie completat si link
        (*newNode)->link[j - median] = node->link[j];
        j += 1;
    }

    // Update num_keyss
    node->num_keys = median;
    (*newNode)->num_keys = ROW_MAX_KEYS - median;
    //functie de request a unei pagini goale
    (*newNode)->page_num = node->page_num + 1;

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
    (*newNode)->link[0] = node->link[node->num_keys];
    node->num_keys--;
}


// Function to determine where to insert a value in the B-tree
// Parameters:
// - val: The value to insert
// - pval: Pointer to store the promoted value
// - node: Pointer to the current node being checked
// - child: Pointer to store the new child node if split occurs
int set_value(const int val, int *pval, struct RowNode *node,struct RowNode **child){
    int pos;

    //node receive initially root pointer
    // if tree is empty, create new root
    if(!node){
        *pval = val, *child = NULL;
        return 1;
    }

    // finding correct position for the value in the current node
    if (val < node->keys[1]) {
        pos = 0;
    } else {
        for (pos = node->num_keys; (val < node->keys[pos] && pos > 1); pos--)
            ;

        // Prevent duplicate values
        // if (val == node->keys[pos]) {
        //     printf("Duplicates are not permitted\n");
        //     return 0;
        // }
    }

    // Recursively insert into the correct subtree
    if (set_value(val, pval, node->plink[pos], child)) {
        // If node has space, insert normally
        if (node->num_keys < ROW_MAX_KEYS) {
            //nodul nou inserat ramane in aceeasi pagina
            insert_node(*pval, pos, node, *child);
        } else {
            // Otherwise, split the node
            //aici o sa se schimbe numarul paginii
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
void search(const int val, int *pos, struct RowNode *myNode) {
    if (!myNode) {
        printf("%d not found in the tree\n", val);
        return;
    }

    // Find the correct position
    if (val < myNode->keys[1]) {
        *pos = 0;
    } else {
        for (*pos = myNode->num_keys; (val < myNode->keys[*pos] && *pos > 1); (*pos)--)
            ;

        // If found, print message and return
        if (val == myNode->keys[*pos]) {
            printf("%d is found\n", val);
            return;
        }
    }
    // Recursively search in the correct subtree
    search(val, pos, myNode->plink[*pos]);
}


// Function to perform an in-order traversal of the B-tree
// Parameter:
// - myNode: Pointer to the node being traversed
void traversal(const struct RowNode *myNode) {
    if (myNode) {
        int i;

        // Visit left subtree, print value, then right subtree
        for (i = 0; i < myNode->num_keys; i++) {
            traversal(myNode->plink[i]);
            printf("Node num %ld, ", myNode->keys[i + 1]);
            printf("Node page %ld \n", myNode->page_num);
            printf("Node link %ld %ld %ld\n", myNode->link[0],myNode->link[1],myNode->link[2]);
        }

        traversal(myNode->plink[i]);
    }
}


// Function to find the in-order predecessor (largest value in left subtree)
// Parameter:
// - node: Pointer to the subtree where predecessor is searched
// Returns: The predecessor value
int find_predecessor(const struct RowNode *node) {
    while (node->plink[node->num_keys] != NULL)
        node = node->plink[node->num_keys];  // Move to the rightmost child
    return node->keys[node->num_keys];
}

// Function to find the in-order successor (smallest value in right subtree)
// Parameter:
// - node: Pointer to the subtree where successor is searched
// Returns: The successor value
int find_successor(const struct RowNode *node) {
    while (node->plink[0] != NULL)
        node = node->plink[0];  // Move to the leftmost child
    return node->keys[1];
}

// Function to shift values and child pointers left after deletion
// This function is used when a value is removed from a node, ensuring
// that all remaining values are shifted left to maintain order.
// Parameters:
// - node: Pointer to the node where shifting occurs
// - pos: The position from where values and child pointers need to be shifted
void shift_left(struct RowNode *node, const int pos) {
    for (int i = pos; i < node->num_keys; i++) {
        node->keys[i] = node->keys[i + 1];
        node->plink[i] = node->plink[i + 1];
    }
    node->plink[node->num_keys] = node->plink[node->num_keys + 1];
    node->num_keys--;
}

// Function to delete a value from the B-tree
// Parameters:
// - node: Pointer to the current node being checked
// - val: The value to delete
void delete_value(struct RowNode *node, const int val) {
    int pos;

    // Find position of value in current node
    if (val < node->keys[1]) {
        pos = 0;
    } else {
        for (pos = node->num_keys; (val < node->keys[pos] && pos > 1); pos--);
    }

    // If value is found in this node
    if (val == node->keys[pos]) {
        if (node->plink[pos - 1] == NULL) {
            // Case 1: Leaf node - Simply remove the value
            shift_left(node, pos);
        } else {
            // Case 2: Internal node - Replace with predecessor or successor
            if (node->plink[pos - 1] != NULL) {
                const int pred = find_predecessor(node->plink[pos - 1]);
                node->keys[pos] = pred;
                delete_value(node->plink[pos - 1], pred);
            } else {
                const int succ = find_successor(node->plink[pos]);
                node->keys[pos] = succ;
                delete_value(node->plink[pos], succ);
            }
        }
    } else {
        // If value is not found in this node, search in the correct subtree
        if (node->plink[pos] == NULL) {
            printf("Value %d not found in the tree\n", val);
            return;
        }
        delete_value(node->plink[pos], val);
    }

    // If node becomes underfull, handle merging or borrowing (not implemented fully here)
}

// Function to delete a value from the B-tree
// Parameter:
// - val: The value to delete
void delete_value_from_tree(const int val) {
    if (root == NULL) {
        printf("Tree is empty\n");
        return;
    }

    delete_value(root, val);

    // If root has no values left, adjust the root (if tree shrinks)
    if (root->num_keys == 0) {
        struct RowNode *temp = root;
        root = root->plink[0];
        free(temp);
    }
}

// Function to insert a value into the B-tree
// Parameter:
// - val: The value to insert
void insert(const int val){
    int i;
    struct RowNode *child;

    const int flag = set_value(val, &i, root, &child);

    if(flag)
        root = create_node(i, child);

    printf("%ld\n",root->page_num);
}

#endif /* BF5ED1DD_D1BD_4342_B75A_C25BB21B57A4 */
