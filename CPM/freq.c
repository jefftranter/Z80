/*
 * Print frequency of occurrence of characters in files.
 * The original version was written by ChatGPT :-)
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHAR 256

int main(int argc, char *argv[])
{
    char *filename;
    FILE *fp;
    int i, j, c;
    int count[MAX_CHAR];

    if (argc < 2) {
        printf("Usage: freq <filename(s)>\n");
        return 1;
    }

    for (j = 1; j < argc; j++) {
        filename = argv[j];

        memset(count, 0, sizeof(count));

        fp = fopen(filename, "r");
        if (!fp) {
            printf("Unable to open file: %s\n", filename);
            return 1;
        }

        while ((c = fgetc(fp)) != EOF) {
            count[c]++;
        }

        fclose(fp);

        printf("\nFile: %s\n", filename);

        printf("Character\tOccurrence\n");
        printf("---------\t----------\n");

        for (i = 0; i < MAX_CHAR; i++) {
            if (count[i] > 0) {
                if (isprint(i)) {
                    printf("'%c'\t\t%d\n", i, count[i]);
                } else {
                    printf("%d\t\t%d\n", i, count[i]);
                }
            }
        }
    }

    return 0;
}
