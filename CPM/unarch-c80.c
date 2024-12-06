/*

  Simple file unarchiver. Extracts files created by arch.c. Runs on
  Linux, CP/M, and HDOS. This version compiles under HDOS or CP/M
  using the Software Toolworks C/80 compiler.

  To build on HDOS:
  c80 -d1000 sy4:unarch=sy4:unarch
  a80 sy4:unarch=sy4:unarch

  To build on CP/M:
  c80 -d1000 c::unarch=c:unarch
  a80 c:unarch=c:unarch

*/

#include "stdio.h"

/* Maximum number of files supported */
#define MAX_FILES 16

/*
  These two functions are not available under HDOS or CP/M so are
  implemented here.
*/
int fread(ptr, size, nmemb, fp)
    char *ptr; int size; int nmemb; FILE *fp;
{
    int i;

    for (i = 0; i < size * nmemb; i++) {
        ptr[i] = getc(fp);
    }

    return 0;
}

int fwrite(ptr, size, nmemb, fp)
    char *ptr; int size; int nmemb; FILE *fp;
{
    int i;

    for (i = 0; i < size * nmemb; i++) {
        putc(ptr[i], fp);
    }

    return 0;
}

int main(argc, argv)
    int argc; char **argv;
{
    int numfiles;
    int i, j, k, n;
    FILE *afp;
    FILE *fp;
    char buffer[256];
    char filename[MAX_FILES][12];
    int size[MAX_FILES];

    /* Check command line options. */
    if (argc !=2) {
        fprintf(stderr, "Usage: %s <archive file>\n", argv[0]);
        return 1;
    }

    /* Open archive file. */
    afp = fopen(argv[1], "rb");
    if (afp == NULL) {
        fprintf(stderr, "Error: open of %s failed\n", argv[1]);
        return 1;
    }

    /* Read and verify magic number. */
    fread(buffer, 1, 3, afp);
    if (buffer[0] != 'J' && buffer[1] != 'J' && buffer[2] != 'T') {
        fprintf(stderr, "Error: %s is not a valid archive file\n", argv[1]);
        return 1;
    }

    /* Read number of files in archive. */
    fread(buffer, 1, 1, afp);
    numfiles = buffer[0];
    if (numfiles > MAX_FILES) {
        fprintf(stderr, "Error: too many files in archive\n");
        return 1;
    }

    /* For each file: read filename and length  */
    for (i = 0; i < numfiles; i++) {
        fread(buffer, 1, 12, afp);
        strcpy(filename[i], buffer);
        fread(buffer, 1, 4, afp);
        /* Below is a hack to convert signed chars to unsigned integers. */
        j = (buffer[2] < 0) ? ~buffer[2] + 1 : buffer[2];
        k = (buffer[3] < 0) ? ~buffer[3] + 1 : buffer[3];
        size[i] = (j << 8) + k;
    }

    /* for each file:
         open output file
         read raw data based on length
         write data to file
         close file
    */
    for (i = 0; i < numfiles; i++) {
        fp = fopen(filename[i], "wb");
        if (fp == NULL) {
            fprintf(stderr, "Error: unable to create %s\n", filename[i]);
            return 1;
        }

        fprintf(stderr, "Extracting %s (%d bytes)\n", filename[i], size[i]);

        n = size[i];
        do {
            if (n > 256) {
                fread(buffer, 1, 256, afp);
                fwrite(buffer, 1, 256, fp);
                n = n - 256;
            } else {
                fread(buffer, 1, n, afp);
                fwrite(buffer, 1, n, fp);
                n = 0;
            }

        } while (n > 0);

        fclose(fp);
    }

    /* Verify end of archive marker */
    fread(buffer, 1, 3, afp);
    if (buffer[0] != 'E' && buffer[1] != 'N' && buffer[2] != 'D') {
        fprintf(stderr, "Warning: No end of archive file marker found\n");
    }

    /* Close archive file. */
    fclose(afp);

    return 0;
}

#include "adlib.c"
#include "stdlib.c"
#include "printf.c"
