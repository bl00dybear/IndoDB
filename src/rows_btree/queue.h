#ifndef F1EC1245_5B68_4E95_AE9A_5831D696CE9C
#define F1EC1245_5B68_4E95_AE9A_5831D696CE9C

#include "data_structures.h"
Queue* create_queue() {
    Queue* queue = (Queue*)malloc(sizeof(Queue));
    queue->front = queue->rear = NULL;
    return queue;
}

// Verifică dacă coada este goală
bool is_empty(Queue* queue) {
    return (queue->front == NULL);
}

// Adaugă un element în coadă (enqueue)
void push(Queue* queue, int value) {
    QueueNode* new_queue_node = (QueueNode*)malloc(sizeof(QueueNode));
    if (!new_queue_node) {
        printf("Eroare: Memorie insuficientă!\n");
        return;
    }
    new_queue_node->data = value;
    new_queue_node->next = NULL;

    if (queue->rear == NULL) {  // Dacă coada este goală
        queue->front = queue->rear = new_queue_node;
    } else {
        queue->rear->next = new_queue_node;
        queue->rear = new_queue_node;
    }
    printf("Elementul %d a fost adăugat în coadă.\n", value);
}

// Elimină un element din coadă (dequeue)
int pop(Queue* queue) {
    if (is_empty(queue)) {
        printf("Eroare: Coada este goală!\n");
        return -1;
    }
    QueueNode* temp = queue->front;
    int removed_value = temp->data;
    queue->front = queue->front->next;

    if (queue->front == NULL) {  // Dacă coada devine goală
        queue->rear = NULL;
    }

    free(temp);
    return removed_value;
}

// Returnează primul element fără a-l elimina (peek)
int front(Queue* queue) {
    if (is_empty(queue)) {
        printf("Eroare: Coada este goală!\n");
        return -1;
    }
    return queue->front->data;
}

// Afișează elementele cozii
void display(Queue* queue) {
    if (is_empty(queue)) {
        printf("Coada este goală!\n");
        return;
    }
    QueueNode* temp = queue->front;
    printf("Elementele cozii: ");
    while (temp != NULL) {
        printf("%d ", temp->data);
        temp = temp->next;
    }
    printf("\n");
}

// Eliberarea memoriei cozii
void free_queue(Queue* queue) {
    while (!is_empty(queue)) {
        pop(queue);
    }
    free(queue);
}

#endif /* F1EC1245_5B68_4E95_AE9A_5831D696CE9C */
