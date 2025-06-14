Prompt: Scrie un cod in C care defineste niste constante (buffer size si niste path-uri) si are functii pentru: copiere fisier, copiere director recursiv, stergere director recursiv si redenumire director. foloseste doar apeluri POSIX si afiseaza erorile cu fprintf(stderr, ...).

## Raspuns:

Iată un exemplu complet de cod C care:

- definește constante pentru buffer size și câteva path-uri;
- implementează **copierea unui fișier**, **copierea recursivă a unui director**, **ștergerea recursivă a unui director** și **redenumirea unui director**;
- utilizează doar **apeluri POSIX** (`open`, `read`, `write`, `opendir`, `readdir`, `mkdir`, `unlink`, `rmdir`, `rename`, `stat`, etc.);
- afișează erorile cu `fprintf(stderr, ...)`.

```c
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <errno.h>

#define BUFFER_SIZE 4096
#define SRC_PATH "./src"
#define DST_PATH "./dst"
#define TMP_PATH "./tmp"

// Copiază un fișier (doar conținut, nu metadate speciale)
int copy_file(const char *src_file, const char *dst_file) {
    int src_fd = open(src_file, O_RDONLY);
    if (src_fd < 0) {
        fprintf(stderr, "Eroare la deschiderea fișierului sursă '%s': %s\n", src_file, strerror(errno));
        return -1;
    }

    int dst_fd = open(dst_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (dst_fd < 0) {
        fprintf(stderr, "Eroare la deschiderea fișierului destinație '%s': %s\n", dst_file, strerror(errno));
        close(src_fd);
        return -1;
    }

    char buffer[BUFFER_SIZE];
    ssize_t bytes_read, bytes_written;

    while ((bytes_read = read(src_fd, buffer, BUFFER_SIZE)) > 0) {
        bytes_written = write(dst_fd, buffer, bytes_read);
        if (bytes_written != bytes_read) {
            fprintf(stderr, "Eroare la scriere în '%s': %s\n", dst_file, strerror(errno));
            close(src_fd);
            close(dst_fd);
            return -1;
        }
    }

    if (bytes_read < 0) {
        fprintf(stderr, "Eroare la citire din '%s': %s\n", src_file, strerror(errno));
    }

    close(src_fd);
    close(dst_fd);
    return 0;
}

// Creează un director (dacă nu există deja)
int ensure_directory(const char *path) {
    struct stat st;
    if (stat(path, &st) == -1) {
        if (mkdir(path, 0755) == -1) {
            fprintf(stderr, "Eroare la creare director '%s': %s\n", path, strerror(errno));
            return -1;
        }
    } else if (!S_ISDIR(st.st_mode)) {
        fprintf(stderr, "'%s' există dar nu este un director\n", path);
        return -1;
    }
    return 0;
}

// Copiază recursiv un director
int copy_directory(const char *src_dir, const char *dst_dir) {
    DIR *dir = opendir(src_dir);
    if (!dir) {
        fprintf(stderr, "Eroare la deschiderea directorului '%s': %s\n", src_dir, strerror(errno));
        return -1;
    }

    if (ensure_directory(dst_dir) != 0) {
        closedir(dir);
        return -1;
    }

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        char src_path[PATH_MAX], dst_path[PATH_MAX];
        snprintf(src_path, sizeof(src_path), "%s/%s", src_dir, entry->d_name);
        snprintf(dst_path, sizeof(dst_path), "%s/%s", dst_dir, entry->d_name);

        struct stat st;
        if (stat(src_path, &st) == -1) {
            fprintf(stderr, "Eroare la stat('%s'): %s\n", src_path, strerror(errno));
            continue;
        }

        if (S_ISDIR(st.st_mode)) {
            if (copy_directory(src_path, dst_path) != 0)
                return -1;
        } else if (S_ISREG(st.st_mode)) {
            if (copy_file(src_path, dst_path) != 0)
                return -1;
        }
    }

    closedir(dir);
    return 0;
}

// Șterge recursiv un director
int delete_directory(const char *path) {
    DIR *dir = opendir(path);
    if (!dir) {
        fprintf(stderr, "Eroare la deschiderea directorului '%s': %s\n", path, strerror(errno));
        return -1;
    }

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        char full_path[PATH_MAX];
        snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);

        struct stat st;
        if (stat(full_path, &st) == -1) {
            fprintf(stderr, "Eroare la stat('%s'): %s\n", full_path, strerror(errno));
            continue;
        }

        if (S_ISDIR(st.st_mode)) {
            if (delete_directory(full_path) != 0)
                return -1;
        } else {
            if (unlink(full_path) == -1) {
                fprintf(stderr, "Eroare la ștergerea fișierului '%s': %s\n", full_path, strerror(errno));
                return -1;
            }
        }
    }

    closedir(dir);

    if (rmdir(path) == -1) {
        fprintf(stderr, "Eroare la ștergerea directorului '%s': %s\n", path, strerror(errno));
        return -1;
    }

    return 0;
}

// Redenumește un director
int rename_directory(const char *old_path, const char *new_path) {
    if (rename(old_path, new_path) == -1) {
        fprintf(stderr, "Eroare la redenumirea directorului '%s' în '%s': %s\n", old_path, new_path, strerror(errno));
        return -1;
    }
    return 0;
}

// Exemplu de utilizare
int main() {
    // Copiere recursivă
    if (copy_directory(SRC_PATH, DST_PATH) == 0) {
        printf("Copiere reușită din '%s' în '%s'.\n", SRC_PATH, DST_PATH);
    }

    // Redenumire director
    if (rename_directory(DST_PATH, TMP_PATH) == 0) {
        printf("Redenumire reușită: '%s' -> '%s'\n", DST_PATH, TMP_PATH);
    }

    // Ștergere director recursiv
    if (delete_directory(TMP_PATH) == 0) {
        printf("Directorul '%s' a fost șters cu succes.\n", TMP_PATH);
    }

    return 0;
}
```

### Note:

- Poți ajusta `SRC_PATH`, `DST_PATH`, `TMP_PATH` pentru a testa diferite directoare.
- Codul tratează doar fișiere obișnuite și directoare. Nu procesează linkuri simbolice sau fișiere speciale.
- Verifică existența directoarelor de test (`./src`, etc.) înainte de rulare.

Vrei și o variantă separată pentru fișiere ascunse sau linkuri simbolice?
