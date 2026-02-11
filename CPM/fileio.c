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
    static char *filename = "TEST.TXT";
    static FILE *fp;
    static char c;
    static int i, rc;
    static char buffer[80];

    remove(filename);
    //rename("TEST.TXT", "NEW.TXT");

    printf("Opening file %s for write...\n", filename);
    fp = fopen(filename, "w");
    if (fp == NULL) {
        printf("Error: unable to open '%s'\n", filename);
        return 1;
    }

    for (i = 1; i <= 10; i++) {
        printf("Writing file...\n");
        fprintf(fp, "Data line %02d\n", i);
    }

    printf("Closing file...\n");
    rc = fclose (fp);
    if (rc != 0) {
        printf("fclose returned status %d\n", rc);
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
    while (1) {
        c = getc(fp);
        if (c == EOF)
            break;
        printf("read: '%c'\n", c);
    }
#endif

#if 1
    /* Implementation using fgets */
    printf("Reading file...\n");
    for (i = 1; i <= 10; i++) {
        if (feof(fp))
            break;
        fgets(buffer, sizeof(buffer)-1, fp);
        printf("%s", buffer);
    }
#endif

    printf("Closing file...\n");
    rc = fclose (fp);
    if (rc != 0) {
        printf("fclose returned status %d\n", rc);
        return 1;
    }

    printf("Done.\n");
    return 0;
}
