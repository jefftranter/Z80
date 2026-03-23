/* Demo of reading directory and doing wildcard matching */

#include <libgen.h>
#include <stdio.h>
#include <string.h>

/* Match directory entries against a glob pattern */
void match_entry(const char *entry, char *pattern) {
    int i, j;
    char filename[13];

    /* Check for blank or deleted entry */
    if ((entry[0] == 0) || (entry[0] == -1) || (entry[0] == -2)) {
        return;
    }

    j = 0;

    /* Copy valid characters of filename */
    for (i = 0; i < 8; i++) {
        if (entry[i] == 0) {
            break;
        }
        filename[j] = entry[i];
        j++;
    }

    // Put dot between filename and extension.
    filename[j] = '.';
    j++;

    /* Add extension. */
    for (i = 8; i < 12; i++) {
        if (entry[i] == 0) {
            break;
        }
        filename[j] = entry[i];
        j++;
    }

    // Terminating null */
    filename[j] = 0;

    printf("%12s   ", filename);
    glob(filename, pattern) ? printf("Y") : printf("N");
    printf("      ");
    glob_dos(filename, pattern) ? printf("Y") : printf("N");
    printf("        ");
    glob_fat(filename, pattern) ? printf("Y") : printf("N");
    printf("\n");
}

/* Read a DIRECT.SYS file and iterate over each directory entry */
void loop_dir(char *pattern) {
    char buffer[23];
    int reads = 0;

    FILE *file = fopen("DIRECT.SYS", "rb"); // Open the file in binary mode
    if (file == NULL) {
        perror("Failed to open DIRECT.SYS");
        return;
    }

    printf("Filename      glob glob_dos glob_fat\n");
    printf("------------  ---- -------- --------\n");

    while (fread(buffer, 1, 23, file) > 0) {
        match_entry(buffer, pattern);
        reads++;
        if (reads % 22 == 0) {
            /* Read 6 padding chars */
            fread(buffer, 1, 6, file);
        }
    }

    fclose(file);
}

int main(int argc, char *argv[]) {

    //if (argc != 2) {
    //    printf("usage: wildcard <pattern>\n");
    //    return 1;
    //}

    //loop_dir(argv[1]);
    loop_dir("*.ABS");
    loop_dir("*.SYS");

    return 0;
}

