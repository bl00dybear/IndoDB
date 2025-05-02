#ifndef QUEUE_H
#define QUEUE_H

#include "../data/core_structures.h"

Queue* create_queue();
bool is_empty(Queue* queue);
void push(Queue* queue, int value);
int pop(Queue* queue);
int front(Queue* queue);
void display(Queue* queue);
void destroy_queue(Queue* queue);

#endif
