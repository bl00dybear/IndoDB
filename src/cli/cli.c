#include "../include/cli/cli.h"


void disable_raw_mode() {
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

void enable_raw_mode() {
    tcgetattr(STDIN_FILENO, &orig_termios);
    atexit(disable_raw_mode);
    struct termios raw = orig_termios;
    raw.c_lflag &= ~(ECHO | ICANON | ISIG);
    raw.c_iflag &= ~(IXON | ICRNL);
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

int is_exit_or_quit(const char *line) {
    char lower[MAX_INPUT_SIZE];
    size_t len = strlen(line);
    for (size_t i = 0; i < len && i < MAX_INPUT_SIZE - 1; ++i) {
        lower[i] = tolower(line[i]);
    }
    lower[len] = '\0';

    if (strcmp(lower, "exit") == 0 || strcmp(lower, "exit;") == 0 ||
        strcmp(lower, "quit") == 0 || strcmp(lower, "quit;") == 0) {
        return 1;
    }
    return 0;
}

int read_line(char *buf, size_t size) {
    int len = 0, pos = 0, hist_pos = hist_len;
    char c;
    enable_raw_mode();
    while (1) {
        if (read(STDIN_FILENO, &c, 1) != 1) {
            disable_raw_mode();
            return 0; // EOF
        }
        if (c == '\r' || c == '\n') {
            write(STDOUT_FILENO, "\r\n", 2);
            buf[len] = '\0';
            if (len > 0 && hist_len < MAX_HISTORY) {
                history[hist_len++] = strdup(buf);
            }
            disable_raw_mode();
            return 1;
        } else if (c == 127) {
            if (pos > 0) {
                memmove(buf + pos - 1, buf + pos, len - pos);
                len--; pos--;
                write(STDOUT_FILENO, "\x1b[D", 3);
                write(STDOUT_FILENO, buf + pos, len - pos);
                write(STDOUT_FILENO, " ", 1);
                for (int i = 0; i <= len - pos; i++) write(STDOUT_FILENO, "\x1b[D", 3);
            }
        } else if (c == '\x1b') {
            char seq[2];
            if (read(STDIN_FILENO, &seq[0], 1) != 1) continue;
            if (read(STDIN_FILENO, &seq[1], 1) != 1) continue;
            if (seq[0] == '[') {
                if (seq[1] == 'C' && pos < len) { write(STDOUT_FILENO, "\x1b[C", 3); pos++; }
                else if (seq[1] == 'D' && pos > 0) { write(STDOUT_FILENO, "\x1b[D", 3); pos--; }
                else if (seq[1] == 'A' && hist_pos > 0) {
                    hist_pos--;
                    // clear line
                    while (pos--) write(STDOUT_FILENO, "\x1b[D", 3);
                    for (int i = 0; i < len; i++) write(STDOUT_FILENO, " ", 1);
                    for (int i = 0; i < len; i++) write(STDOUT_FILENO, "\x1b[D", 3);
                    const char *h = history[hist_pos];
                    len = pos = strlen(h);
                    strcpy(buf, h);
                    write(STDOUT_FILENO, buf, len);
                } else if (seq[1] == 'B' && hist_pos < hist_len) {
                    hist_pos++;
                    // clear line
                    while (pos--) write(STDOUT_FILENO, "\x1b[D", 3);
                    for (int i = 0; i < len; i++) write(STDOUT_FILENO, " ", 1);
                    for (int i = 0; i < len; i++) write(STDOUT_FILENO, "\x1b[D", 3);
                    if (hist_pos < hist_len) {
                        const char *h = history[hist_pos];
                        len = pos = strlen(h);
                        strcpy(buf, h);
                        write(STDOUT_FILENO, buf, len);
                    } else {
                        len = pos = 0;
                        buf[0] = '\0';
                    }
                }
            }
        } else if (c >= 32 && c <= 126) {
            if (len < (int)size - 1) {
                memmove(buf + pos + 1, buf + pos, len - pos);
                buf[pos] = c;
                write(STDOUT_FILENO, buf + pos, len - pos + 1);
                len++; pos++;
                for (int i = 0; i < len - pos; i++) write(STDOUT_FILENO, "\x1b[D", 3);
            }
        }
    }
}

int cli() {
    char input[MAX_INPUT_SIZE];
    char line[MAX_INPUT_SIZE];

    // printf("\033[H\033[J");

    while (1) {
        printf("IndoDB> ");
        fflush(stdout);
        input[0] = '\0';

        while (read_line(line, sizeof(line))) {
            size_t len = strlen(line);
            if (len > 0 && line[len - 1] == '\n') {
                line[len - 1] = '\0'; len--;
            }

            if (is_exit_or_quit(line)) {
                printf("Exiting IndoDB...\n");
                fflush(stdout);
                return 0;
            }

            if (strlen(line) == 0 && strlen(input) == 0) {
                // Linia e goală și nu avem input acumulat — restart prompt
                break;
            }

            if (strlen(input) + strlen(line) + 2 < MAX_INPUT_SIZE) {
                strcat(input, line);
                strcat(input, " ");
            } else {
                printf("Input too long! Max supported length is %d characters.\n", MAX_INPUT_SIZE);
                break;
            }

            char *trimmed = input + strlen(input) - 1;
            while (trimmed >= input && *trimmed == ' ') trimmed--;
            if (*trimmed == ';') break;

            printf("     -> ");
            fflush(stdout);
        }

        if (strlen(input) > 0) {
            FILE *fp = popen("../output/sql_parser", "w");
            if (fp == NULL) {
                perror("Error opening pipe");
                return 1;
            }
            fprintf(fp, "%s\n", input);
            pclose(fp);


            Statement *stmt = malloc(sizeof(Statement));
            if (!stmt) {
                perror("malloc failed");
                exit(1);
            }
            int res = parse_statement("../output/output.json", stmt);
            
            if(res == -1){
                free_statement(stmt);
                continue;
            }


            if (stmt != NULL) {
                process_statement(stmt);
                free_statement(stmt);
            }else {
                printf("Failed to parse statement!\n");
                exit(EXIT_FAILURE);
            }

        }
    }
}