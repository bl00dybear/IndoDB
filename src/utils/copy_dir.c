
#define _XOPEN_SOURCE 700
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>

#define COPY_BUFFER_SIZE  (64 * 1024)
#define PATH_MAX 4096


// utils for file management -> ignore their internal process
static int copy_file(const char *src_path, const char *dst_path, mode_t mode) {
    int in_fd  = -1, out_fd = -1;
    ssize_t nread;
    char *buf = NULL;
    int ret = -1;

    buf = malloc(COPY_BUFFER_SIZE);
    if (!buf) {
        fprintf(stderr, "malloc failed: %s\n", strerror(errno));
        goto cleanup;
    }

    in_fd = open(src_path, O_RDONLY);
    if (in_fd < 0) {
        fprintf(stderr, "open \"%s\" for reading failed: %s\n", src_path, strerror(errno));
        goto cleanup;
    }

    out_fd = open(dst_path, O_WRONLY | O_CREAT | O_TRUNC, mode);
    if (out_fd < 0) {
        fprintf(stderr, "open \"%s\" for writing failed: %s\n", dst_path, strerror(errno));
        goto cleanup;
    }

    while ((nread = read(in_fd, buf, COPY_BUFFER_SIZE)) > 0) {
        char *out_ptr = buf;
        ssize_t nwritten;

        do {
            nwritten = write(out_fd, out_ptr, nread);
            if (nwritten >= 0) {
                nread -= nwritten;
                out_ptr += nwritten;
            } else if (errno != EINTR) {
                fprintf(stderr, "write error to \"%s\": %s\n", dst_path, strerror(errno));
                goto cleanup;
            }
        } while (nread > 0);
    }

    if (nread < 0) {
        fprintf(stderr, "read error from \"%s\": %s\n", src_path, strerror(errno));
        goto cleanup;
    }

    ret = 0;

cleanup:
    if (in_fd >= 0)  close(in_fd);
    if (out_fd >= 0) close(out_fd);
    free(buf);
    return ret;
}

static int copy_directory(const char *src_dir, const char *dst_dir) {
    DIR *dir = NULL;
    struct dirent *entry;
    struct stat statbuf;
    char src_path[PATH_MAX + 1];
    char dst_path[PATH_MAX + 1];
    int ret = -1;

    if (lstat(src_dir, &statbuf) < 0) {
        fprintf(stderr, "lstat \"%s\" failed: %s\n", src_dir, strerror(errno));
        return -1;
    }
    if (!S_ISDIR(statbuf.st_mode)) {
        fprintf(stderr, "\"%s\" is not a directory.\n", src_dir);
        return -1;
    }

    if (mkdir(dst_dir, statbuf.st_mode & 0777) < 0) {
        if (errno != EEXIST) {
            fprintf(stderr, "mkdir \"%s\" failed: %s\n", dst_dir, strerror(errno));
            return -1;
        }
    }

    dir = opendir(src_dir);
    if (!dir) {
        fprintf(stderr, "opendir \"%s\" failed: %s\n", src_dir, strerror(errno));
        return -1;
    }

    errno = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        snprintf(src_path, sizeof(src_path), "%s/%s", src_dir, entry->d_name);
        snprintf(dst_path, sizeof(dst_path), "%s/%s", dst_dir, entry->d_name);

        if (lstat(src_path, &statbuf) < 0) {
            fprintf(stderr, "lstat \"%s\" failed: %s\n", src_path, strerror(errno));
            goto cleanup;
        }

        if (S_ISDIR(statbuf.st_mode)) {
            if (copy_directory(src_path, dst_path) < 0) {
                goto cleanup;
            }
        }
        else if (S_ISREG(statbuf.st_mode)) {
            if (copy_file(src_path, dst_path, statbuf.st_mode & 0777) < 0) {
                goto cleanup;
            }
        }
        else if (S_ISLNK(statbuf.st_mode)) {
            char link_target[PATH_MAX + 1];
            ssize_t len = readlink(src_path, link_target, sizeof(link_target) - 1);
            if (len < 0) {
                fprintf(stderr, "readlink \"%s\" failed: %s\n", src_path, strerror(errno));
                goto cleanup;
            }
            link_target[len] = '\0';

            if (symlink(link_target, dst_path) < 0) {
                fprintf(stderr, "symlink from \"%s\" to \"%s\" failed: %s\n",
                        link_target, dst_path, strerror(errno));
                goto cleanup;
            }
        }
        else {
            fprintf(stderr, "Skipping unsupported file type: \"%s\"\n", src_path);
        }
    }

    if (errno != 0) {
        fprintf(stderr, "readdir error in \"%s\": %s\n", src_dir, strerror(errno));
        goto cleanup;
    }

    ret = 0;

cleanup:
    closedir(dir);
    return ret;
}

int remove_directory(const char *path) {
    DIR *dir = NULL;
    struct dirent *entry;
    struct stat statbuf;
    char fullpath[PATH_MAX + 1];
    int ret = -1;

    if (lstat(path, &statbuf) < 0) {
        fprintf(stderr, "lstat(\"%s\") failed: %s\n", path, strerror(errno));
        return -1;
    }
    if (!S_ISDIR(statbuf.st_mode)) {
        if (unlink(path) < 0) {
            fprintf(stderr, "unlink(\"%s\") failed: %s\n", path, strerror(errno));
            return -1;
        }
        return 0;
    }

    dir = opendir(path);
    if (!dir) {
        fprintf(stderr, "opendir(\"%s\") failed: %s\n", path, strerror(errno));
        return -1;
    }

    errno = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0)
        {
            continue;
        }

        snprintf(fullpath, sizeof(fullpath), "%s/%s", path, entry->d_name);

        if (lstat(fullpath, &statbuf) < 0) {
            fprintf(stderr, "lstat(\"%s\") failed: %s\n", fullpath, strerror(errno));
            goto cleanup;
        }

        if (S_ISDIR(statbuf.st_mode)) {
            if (remove_directory(fullpath) < 0) {
                goto cleanup;
            }
        }
        else {
            if (unlink(fullpath) < 0) {
                fprintf(stderr, "unlink(\"%s\") failed: %s\n", fullpath, strerror(errno));
                goto cleanup;
            }
        }
    }

    if (errno != 0) {
        fprintf(stderr, "readdir(\"%s\") error: %s\n", path, strerror(errno));
        goto cleanup;
    }

    closedir(dir);
    dir = NULL;

    if (rmdir(path) < 0) {
        fprintf(stderr, "rmdir(\"%s\") failed: %s\n", path, strerror(errno));
        return -1;
    }

    return 0;

cleanup:
    if (dir) closedir(dir);
    return -1;
}

int rename_directory(const char *oldpath, const char *newpath) {
    if (rename(oldpath, newpath) < 0) {
        fprintf(stderr, "rename(\"%s\", \"%s\") failed: %s\n",
                oldpath, newpath, strerror(errno));
        return -1;
    }
    return 0;
}


// actual handlers to use
int create_backup(){
    const char *original_dir = "databases";
    const char *backup_dir   = "backup";

    printf("Starting new transaction...\n");
    if (copy_directory(original_dir, backup_dir) < 0) {
        fprintf(stderr, "Error: failed to copy directory.\n");
        return 0;
    }

    return 1;
}

