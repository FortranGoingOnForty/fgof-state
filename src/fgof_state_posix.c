#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int fgof_state_path_exists(const char *path) {
    struct stat st;

    if (path == NULL || path[0] == '\0') {
        return 0;
    }

    return stat(path, &st) == 0 ? 1 : 0;
}

int fgof_state_directory_exists(const char *path) {
    struct stat st;

    if (path == NULL || path[0] == '\0') {
        return 0;
    }

    if (stat(path, &st) != 0) {
        return 0;
    }

    return S_ISDIR(st.st_mode) ? 1 : 0;
}

int fgof_state_ensure_directory(const char *path, int *error_code) {
    char *buffer;
    char *cursor;
    size_t length;
    struct stat st;

    if (error_code != NULL) {
        *error_code = 0;
    }

    if (path == NULL || path[0] == '\0') {
        if (error_code != NULL) {
            *error_code = EINVAL;
        }
        return 0;
    }

    if (stat(path, &st) == 0) {
        if (S_ISDIR(st.st_mode)) {
            return 1;
        }
        if (error_code != NULL) {
            *error_code = ENOTDIR;
        }
        return 0;
    }

    buffer = strdup(path);
    if (buffer == NULL) {
        if (error_code != NULL) {
            *error_code = ENOMEM;
        }
        return 0;
    }

    length = strlen(buffer);
    while (length > 1 && buffer[length - 1] == '/') {
        buffer[length - 1] = '\0';
        --length;
    }

    for (cursor = buffer + 1; *cursor != '\0'; ++cursor) {
        if (*cursor != '/') {
            continue;
        }

        *cursor = '\0';
        if (buffer[0] != '\0' && mkdir(buffer, 0700) != 0 && errno != EEXIST) {
            if (error_code != NULL) {
                *error_code = errno;
            }
            free(buffer);
            return 0;
        }
        *cursor = '/';
    }

    if (mkdir(buffer, 0700) != 0 && errno != EEXIST) {
        if (error_code != NULL) {
            *error_code = errno;
        }
        free(buffer);
        return 0;
    }

    if (stat(buffer, &st) != 0 || !S_ISDIR(st.st_mode)) {
        if (error_code != NULL) {
            *error_code = errno != 0 ? errno : ENOTDIR;
        }
        free(buffer);
        return 0;
    }

    free(buffer);
    return 1;
}
