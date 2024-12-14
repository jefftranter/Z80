#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>

int main(int argc, char **argv) {
    DIR *dir;
    struct dirent *entry;

    if (argc != 2) {
        printf("usage: %s <directory>\n", argv[0]);
        return 1;
    }
    
    // Open the directory
    dir = opendir(argv[1]);
    if (dir == NULL) {
        perror("opendir");
        return EXIT_FAILURE;
    }

    printf("Files in directory '%s':\n", argv[1]);

    // Read and print each file name in the directory
    while ((entry = readdir(dir)) != NULL) {
        // Ignore the special entries "." and ".."
        if (entry->d_name[0] != '.') {
            printf("%s\n", entry->d_name);
        }
    }

    // Close the directory
    closedir(dir);

    return EXIT_SUCCESS;
}
