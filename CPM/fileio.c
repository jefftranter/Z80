/*

Test program for file i/o.

Open a file for writing
Write several lines to it.
Close file.

Open file for reading.
Read and display lines from it.
Close file.

*/


#include <stdio.h>

int main(int argc, char *argv[])
{
    char *filename = "test.txt";
    FILE *fp;
    char c;
    int i, rc;
    char buffer[80];

    printf("Opening file %s for write...\n", filename);
    fp = fopen(filename, "w");
    if (fp == NULL) {
        printf("Error: unable to open '%s'\n", filename);
        return 1;
    }

    printf("Writing file...\n");
    for (i = 1; i < 100; i++) {
        fprintf(fp, "File data line %02d\n", i);
    }

    printf("Closing file...\n");
    rc = fclose (fp);
    if (rc != 0) {
        printf("fclose returned status %d", rc);
        return 1;
    }

    printf("Opening file %s for read...\n", filename);
    fp = fopen(filename, "r");
    if (fp == NULL) {
        printf("Error: unable to open '%s'\n", filename);
        return 1;
    }

#if 0
    /* Implementation using getc */
    printf("Reading file...\n");
    while ((c = getc(fp)) != EOF) {
        printf("%c", c);
    }
#endif

#if 1
    /* Implementation using fgets */
    printf("Reading file...\n");
    while (!feof(fp)) {
        fgets(buffer, sizeof(buffer)-1, fp);
        printf("%s", buffer);
    }
#endif
    
    printf("Closing file...\n");
    rc = fclose (fp);
    if (rc != 0) {
        printf("fclose returned status %d", rc);
        return 1;
    }

    printf("Done.\n");
    return 0;
}
