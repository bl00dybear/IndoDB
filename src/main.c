#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INPUT_SIZE 1024

int main() {
    char input[MAX_INPUT_SIZE];
    char line[MAX_INPUT_SIZE];
    
    // Clear screen before displaying prompt
    printf("\033[H\033[J");
    
    while (1) {
        printf("IndoDB> ");
        input[0] = '\0';

        while (fgets(line, sizeof(line), stdin)) {
            size_t len = strlen(line);
            if (len > 0 && line[len - 1] == '\n') {
                line[len - 1] = '\0';
                len--;
            }
            
            if (strcmp(line, "EXIT;") == 0) {
                printf("Exiting IndoDB CLI...\n");
                return 0;
            }
            
            strcat(input, line);
            strcat(input, " ");

            // Check if the last non-space character is ';'
            char *trimmed = input + strlen(input) - 1;
            while (trimmed >= input && *trimmed == ' ') {
                trimmed--;
            }
            if (*trimmed == ';') {
                break;
            }
            
            printf("      > ");
        }
        
        if (strlen(input) > 0) {
            FILE *fp = popen("./src/parser/sql_parser", "w");
            if (fp == NULL) {
                perror("Error opening pipe");
                return 1;
            }
            fprintf(fp, "%s\n", input);
            pclose(fp);
        }
    }
    return 0;
}